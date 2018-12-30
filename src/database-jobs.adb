--
--  Database body
--

with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Ada.Environment_Variables;

with Interfaces;

package body Database.Jobs is

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

   function Get_Id (S : SQLite.Statement) return Interfaces.Integer_64 is
      use Interfaces;
   begin
      return SQLite.Column (S, 1);
   end Get_Id;


   function Get_Current_Job return Types.Job_Id is
      use SQLite, Interfaces;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "SELECT Job FROM Current");
   begin
      Command.Step;
      return
        Types.Job_Id (Integer_64'(Command.Column (1)));
   end Get_Current_Job;


   procedure Set_Current_Job (Job : in Types.Job_Id) is
      use SQLite, Interfaces;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "UPDATE Current SET Job=?");
   begin
      Command.Bind (1, Integer_64 (Job));
      Command.Step;
   end Set_Current_Job;


   function Get_Jobs (Top : in Types.Job_Id)
                     return Types.Job_Sets.Vector
   is
      use SQLite;

      Command : Statement;
      Count   : Positive := Positive'First;
      Jobs    : Types.Job_Sets.Vector;
   begin
      Jobs.Clear;
      if Top = All_Jobs then
         Command := Prepare (Database.DB,
                             "SELECT Id, Title " &
                               "FROM Job");
      else
         Command := Prepare (Database.DB,
                             "SELECT Id, Title " &
                               "FROM Job WHERE List = ?");
         Command.Bind (1, Interfaces.Integer_64 (Top));
      end if;

      while Command.Step loop
         declare
            Title_Image : constant String := Column (Command, 2);
            Id          : constant Types.Job_Id
              := Types.Job_Id (Get_Id (Command));
         begin
            Jobs.Append
              ((Id, Ada.Strings.Unbounded.To_Unbounded_String
                  (Title_Image)));
         end;
         Count := Positive'Succ (Count);
      end loop;

      return Jobs;
   end Get_Jobs;


   function Get_Job_Info (Job : in Types.Job_Id)
                         return Types.Job_Info
   is
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
         return Types.Job_Info'(To_Unbounded_String (Title),
                                Types.Job_Id (List),
                                To_Unbounded_String (Owner));
      end;
   end Get_Job_Info;


   procedure Add_Job (Id     : in Types.Job_Id;
                      Title  : in String;
                      Parent : in Types.Job_Id;
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


   function Get_New_Job_Id return Types.Job_Id
   is
      use SQLite;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "SELECT MAX(Id) + 1 FROM Job");
      Id : Types.Job_Id;
   begin
      Command.Step;
      Id := Types.Job_Id (Get_Id (Command));
      return Id;
   end Get_New_Job_Id;


   procedure Transfer (Job       : in Types.Job_Id;
                       To_Parent : in Types.Job_Id)
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


end Database.Jobs;
