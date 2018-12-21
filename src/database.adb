--
--
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

   procedure Get_Current (Job  : out Job_Id;
                          List : out List_Id)
   is
      use SQLite, Interfaces;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "SELECT Job, List " &
                   "FROM Current");
   begin
      Command.Step;
      Job  := Job_Id  (Integer_64'(Command.Column (1)));
      List := List_Id (Integer_64'(Command.Column (2)));
   end Get_Current;

   procedure Get_Jobs (Jobs :    out Job_Set;
                       List : in     List_Id)
   is
      use SQLite;

      Command : Statement;
      Count   : Positive := Positive'First;
   begin
      Jobs.Vector.Clear;
      if List = All_Lists then
         Command := Prepare (Database.DB,
                             "SELECT Id, Title " &
                               "FROM Job");
      else
         Command := Prepare (Database.DB,
                             "SELECT Id, Title " &
                               "FROM Job WHERE List = ?");
         Command.Bind (1, Interfaces.Integer_64 (List));
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

   procedure Get_Lists (Lists : out List_Set) is
      use SQLite;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "SELECT Id, Name, Description " &
                   "FROM List ");
      Count : Positive := Positive'First;
   begin
      Lists.Vector.Clear;
      Lists.Name_Width := 0;
      while Command.Step loop
         declare
            Id         : constant List_Id := List_Id (Get_Id (Command));
            Name_Image : constant String  := Column (Command, 2);
            Desc_Image : constant String  := Column (Command, 3);
         begin
            Lists.Vector.Append (("L" & To_S (Count), Id,
                                  US.To_Unbounded_String (Name_Image),
                                  US.To_Unbounded_String (Desc_Image)));
            Lists.Name_Width := Natural'Max (Lists.Name_Width,
                                             Name_Image'Length);
         end;
         Count := Positive'Succ (Count);
      end loop;
   end Get_Lists;


   procedure Show_Job (Job : in Job_Id) is
      use SQLite, Interfaces, Ada.Text_IO;
      Command_1 : constant Statement :=
        Prepare (Database.DB,
                 "SELECT Title, List, Owner " &
                   "FROM Job " &
                   "WHERE Id=?");
      Command_2 : constant Statement :=
        Prepare (Database.DB,
                 "SELECT Stamp, Kind " &
                   "FROM Event " &
                   "WHERE Job=? " &
                   "ORDER BY Stamp ASC");
   begin
      Command_1.Bind (1, Integer_64 (Job));
      Command_1.Step;
      declare
         Title : constant String     := Command_1.Column (1);
         List  : constant Integer_64 := Command_1.Column (2);
         Owner : constant String     := Command_1.Column (3);
      begin
         Put_Line (Title & " (" & Job'Img & ")");
         Put_Line ("List (" & List'Img & ")  Owner: " & Owner);
      end;

      Command_2.Bind (1, Integer_64 (Job));
      while Command_2.Step loop
         declare
            Stamp : constant String := Command_2.Column (1);
            Kind  : constant String := Command_2.Column (2);
         begin
            Put (Stamp);
            Put ("    ");
            Put (Kind);
            New_Line;
         end;
      end loop;

   end Show_Job;

   procedure Add_Job (Id    : in Job_Id;
                      Title : in String;
                      List  : in List_Id;
                      Owner : in String)
   is
      use SQLite, Interfaces;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "INSERT INTO Job (Id, Title, List, Owner) " &
                   "VALUES (?,?,?,?)");
   begin
      Bind (Command, 1, Integer_64 (Id));
      Bind (Command, 2, Title);
      Bind (Command, 3, Integer_64 (List));
      Bind (Command, 4, Owner);
      Command.Step;
   end Add_Job;

   procedure Create_List (Name : in String)
   is
      use SQLite;
      Command_1 : constant Statement :=
        Prepare (Database.DB,
                 "SELECT MAX(Id) + 1 FROM List");
      Command_2 : constant Statement :=
        Prepare (Database.DB,
                 "INSERT INTO List (Id, Name) " &
                   "VALUES (?,?)");
      Id : Interfaces.Integer_64;
   begin
      Command_1.Step;
      Id := Get_Id (Command_1);

      Command_2.Bind (1, Id);
      Command_2.Bind (2, Name);
      Command_2.Step;
   end Create_List;

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

   function Lookup_List (List : in String) return List_Id is
   begin
      for L of Lists.Vector loop
         --  if Same (L.Ref, List) then
         if L.Ref = List then
            return L.Id;
         end if;
      end loop;
      raise Constraint_Error;
   end Lookup_List;

   function Lookup_Job  (Job  : in String) return Job_Id is
   begin
      for J of Jobs.Vector loop
         --  if Same (J.Ref, Job) then
--         Ada.Text_IO.Put_Line ("#" & J.Ref & "#" & Job & "#");
         if J.Ref = Job then
            return J.Id;
         end if;
      end loop;
      raise Constraint_Error;
   end Lookup_Job;

   procedure Transfer (Job     : in Job_Id;
                       To_List : in List_Id)
   is
      use SQLite, Interfaces;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "UPDATE Job SET List=? " &
                   "WHERE Id=?");
   begin
--        Ada.Text_IO.Put_Line ("Job => " % Job'Img);
--        Ada.Text_IO.Put_Line ("To_List => " % Job'Img);
      Command.Bind (1, Integer_64 (To_List));
      Command.Bind (2, Integer_64 (Job));
      Command.Step;
   end Transfer;

end Database;
