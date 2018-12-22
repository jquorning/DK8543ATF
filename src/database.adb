--
--  Database body
--

with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Environment_Variables;

package body Database is

   use Ada.Strings.Unbounded;

   function "+" (Source : String)
                return Unbounded_String
     renames To_Unbounded_String;

   Database_File_Name : constant String := "todo.db";

   procedure Open is
      Success : Boolean := False;

      procedure Try_Open (File_Name :     String;
                          Success   : out Boolean);

      procedure Try_Open (File_Name :     String;
                          Success   : out Boolean)
      is
         use SQLite, Ada.IO_Exceptions;
      begin
         Success := True;  --  Optimism - result when no exception
         DB := SQLite.Open (File_Name => File_Name,
                            Flags     => READWRITE or FULLMUTEX);
      exception
         when Use_Error =>   --  Could not open database file
            Success := False;
      end Try_Open;

      package Env renames Ada.Environment_Variables;

      Home  : constant String
        := (if Env.Exists ("HOME") then Env.Value ("HOME") else "");

      Paths : constant array (Positive range <>) of Unbounded_String
          := (+"./", +Home & "/etc/", +"/etc/", +Home & "/Work/etc/");


   begin
      for Path of Paths loop

         declare
            Full_Path_Name : constant String
              := To_String (Path) & Database_File_Name;
         begin
            Try_Open (Full_Path_Name, Success);
            if Success then
               return;
            end if;
         end;
      end loop;
      raise Program_Error with "Could not open database file";
   end Open;

   function Get_Id (S : SQLite.Statement) return Interfaces.Integer_64;
   subtype S2 is String (1 .. 2);
   function To_S (A : Natural) return S2;

   function Get_Id (S : SQLite.Statement) return Interfaces.Integer_64 is
      use Interfaces;
   begin
      return SQLite.Column (S, 1);
   end Get_Id;

   function To_S (A : Natural) return S2 is
      package Natural_IO is new Ada.Text_IO.Integer_IO (Positive);
      S : S2;
   begin
      Natural_IO.Default_Width := 2;
      Natural_IO.Put (S, A);
      if S (1) = ' ' then
         S (1) := '0';
      end if;
      return S;
   end To_S;

   function Get_Current_Job return Job_Id is
      use SQLite, Interfaces;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "SELECT Job FROM Current");
   begin
      Command.Step;
      return
        Job_Id (Integer_64'(Command.Column (1)));
   end Get_Current_Job;


   procedure Set_Current_Job (Job : in Job_Id) is
      use SQLite, Interfaces;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "UPDATE Current SET Job=?");
   begin
      Command.Bind (1, Integer_64 (Job));
      Command.Step;
   end Set_Current_Job;


   procedure Get_Jobs (Jobs   :    out Job_Set;
                       Parent : in     Job_Id)
   is
      use SQLite;

      Command : Statement;
      Count   : Positive := Positive'First;
   begin
      Jobs.Vector.Clear;
      if Parent = All_Jobs then
         Command := Prepare (Database.DB,
                             "SELECT Id, Title " &
                               "FROM Job");
      else
         Command := Prepare (Database.DB,
                             "SELECT Id, Title " &
                               "FROM Job WHERE List = ?");
         Command.Bind (1, Interfaces.Integer_64 (Parent));
      end if;

      while Command.Step loop
         declare
            Title_Image : constant String := Column (Command, 2);
            Id          : constant Job_Id := Job_Id (Get_Id (Command));
         begin
            Jobs.Vector.Append (("J" & To_S (Count), Id,
                                 US.To_Unbounded_String (Title_Image)));
         end;
         Count := Positive'Succ (Count);
      end loop;
   end Get_Jobs;

--     procedure Get_Lists (Lists : out List_Set) is
--        use SQLite;
--        Command : constant Statement :=
--          Prepare (Database.DB,
--                   "SELECT Id, Name, Description " &
--                     "FROM List ");
--        Count : Positive := Positive'First;
--     begin
--        Lists.Vector.Clear;
--        Lists.Name_Width := 0;
--        while Command.Step loop
--           declare
--              Id         : constant List_Id := List_Id (Get_Id (Command));
--              Name_Image : constant String  := Column (Command, 2);
--              Desc_Image : constant String  := Column (Command, 3);
--           begin
--              Lists.Vector.Append (("L" & To_S (Count), Id,
--                                    US.To_Unbounded_String (Name_Image),
--                                    US.To_Unbounded_String (Desc_Image)));
--              Lists.Name_Width := Natural'Max (Lists.Name_Width,
--                                               Name_Image'Length);
--           end;
--           Count := Positive'Succ (Count);
--        end loop;
--     end Get_Lists;


   function Get_Job_Info (Job : in Job_Id) return Job_Info is
      use SQLite, Interfaces;

      Command : constant Statement :=
        Prepare (Database.DB,
                 "SELECT Title, List, Owner " &
                   "FROM Job " &
                   "WHERE Id=?");
   begin
      Command.Bind (1, Integer_64 (Job));
      Command.Step;
      declare
         Title : constant String     := Command.Column (1);
         List  : constant Integer_64 := Command.Column (2);
         Owner : constant String     := Command.Column (3);
      begin
         return Job_Info'(To_Unbounded_String (Title),
                          --  List_Id (List),
                          Job_Id (List),
                          To_Unbounded_String (Owner));
      end;
   end Get_Job_Info;


   function Get_Job_Events (Job : in Job_Id)
                           return Event_Lists.Vector
   is
      use SQLite, Interfaces;

      Command : constant Statement :=
        Prepare (Database.DB,
                 "SELECT Stamp, Kind " &
                   "FROM Event " &
                   "WHERE Job=? " &
                   "ORDER BY Stamp ASC");
      Events : Event_Lists.Vector;
   begin
      Command.Bind (1, Integer_64 (Job));
      while Command.Step loop
         declare
            Stamp : constant String := Command.Column (1);
            Kind  : constant String := Command.Column (2);
         begin
            Events.Append ((To_Unbounded_String (Stamp),
                            To_Unbounded_String (Kind)));
         end;
      end loop;
      return Events;
   end Get_Job_Events;


   procedure Add_Job (Id     : in Job_Id;
                      Title  : in String;
                      Parent : in Job_Id;
                      Owner  : in String)
   is
      use SQLite, Interfaces;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "INSERT INTO Job (Id, Title, List, Owner) " &
                   "VALUES (?,?,?,?)");
   begin
      Bind (Command, 1, Integer_64 (Id));
      Bind (Command, 2, Title);
      Bind (Command, 3, Integer_64 (Parent));
      Bind (Command, 4, Owner);
      Command.Step;
   end Add_Job;

--     procedure Create_List (Name : in String)
--     is
--        use SQLite;
--        Command_1 : constant Statement :=
--          Prepare (Database.DB,
--                   "SELECT MAX(Id) + 1 FROM List");
--        Command_2 : constant Statement :=
--          Prepare (Database.DB,
--                   "INSERT INTO List (Id, Name) " &
--                     "VALUES (?,?)");
--        Id : Interfaces.Integer_64;
--     begin
--        Command_1.Step;
--        Id := Get_Id (Command_1);

--        Command_2.Bind (1, Id);
--        Command_2.Bind (2, Name);
--        Command_2.Step;
--     end Create_List;

   function Get_Job_Id return Job_Id
   is
      use SQLite;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "SELECT MAX(Id) + 1 FROM Job");
      Id : Job_Id;
   begin
      Command.Step;
      Id := Job_Id (Get_Id (Command));
      return Id;
   end Get_Job_Id;

--     subtype S3 is String (1 .. 3);
--     function Same (Ref : in S3;
--                    S   : in String) return Boolean;

--     function Same (Ref : in S3;
--                    S   : in String) return Boolean
--     is
--        package Integer_IO is new Ada.Text_IO.Integer_IO (Integer);
--  Trim : constant String  := Ada.Strings.Fixed.Trim (S, Ada.Strings.Both);
--        I_T  : Integer;
--        I_R  : Integer;
--        Last : Natural;
--     begin
--        --  Ada.Text_IO.Put_Line ("#"&Trim&"#"&Ref&"#"&I_T'Img&"#"&I_R'Img);
--        Integer_IO.Get (Trim (Trim'First + 1 .. Trim'Last), Last, I_T);
--        Integer_IO.Get (Ref  (Ref'First  + 1 .. Ref'Last),  Last, I_R);
--        if Ref (Ref'First) = Trim (Trim'First) and I_T = I_R then
--           return True;
--        else
--           return False;
--        end if;
--     end Same;

--     function Lookup_List (List : in String) return List_Id is
--     begin
--        for L of Lists.Vector loop
--           --  if Same (L.Ref, List) then
--           if L.Ref = List then
--              return L.Id;
--           end if;
--        end loop;
--        raise Constraint_Error;
--     end Lookup_List;

--     function Lookup_Job (Job : in String) return Job_Id is
--     begin
--  Ada.Text_IO.Put_Line ("Job to search for : " & Job);
--        for J of Jobs.Vector loop
--           --  if Same (J.Ref, Job) then
--  --         Ada.Text_IO.Put_Line ("#" & J.Ref & "#" & Job & "#");
--  Ada.Text_IO.Put_Line (J.Ref);
--           if J.Ref = Job then
--              return J.Id;
--           end if;
--        end loop;
--        raise Constraint_Error;
--     end Lookup_Job;


   procedure Lookup_Job (Text    : in     String;
                         Job     :    out Job_Id;
                         Success :    out Boolean) is
   begin
      for J of Top_Jobs.Vector loop
         if J.Ref = Text then
            Job     := J.Id;
            Success := True;
            return;
         end if;
      end loop;
      Success := False;
   end Lookup_Job;


   procedure Transfer (Job       : in Job_Id;
                       To_Parent : in Job_Id)
   is
      use SQLite, Interfaces;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "UPDATE Job SET List=? " &
                   "WHERE Id=?");
   begin
      Command.Bind (1, Integer_64 (To_Parent));
      Command.Bind (2, Integer_64 (Job));
      Command.Step;
   end Transfer;


end Database;
