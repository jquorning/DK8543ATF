--
--
--

with SQLite;
with Ada.Text_IO;
with Ada.Calendar;
with Interfaces;
with GNAT.Calendar.Time_IO;

package body Database is

   Database_File_Name : constant String := "todo.db";

   DB : SQLite.Data_Base;

   procedure Open is
   begin
      DB := SQLite.Open (File_Name => Database_File_Name);
   end Open;

   function Get_Id (S : SQLite.Statement) return Natural is
      use Interfaces;
   begin
      return Natural (Integer_64'(SQLite.Column (S, 1)));
   end Get_Id;

   procedure Put_Jobs is
      use SQLite;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "SELECT Id, Title " &
                   "FROM Job ");
   begin
      while Command.Step loop
         declare
            Title_Image : constant String  := Column (Command, 2);
            Id          : constant Natural := Get_Id (Command);
         begin
            Ada.Text_IO.Put (Id'Img);
            Ada.Text_IO.Put ("  ");
            Ada.Text_IO.Put (Title_Image);
            Ada.Text_IO.New_Line;
         end;
      end loop;
   end Put_Jobs;

   procedure Put_Lists is
      use SQLite;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "SELECT Id, Name, Description " &
                   "FROM List ");
   begin
      while Command.Step loop
         declare
            Id         : constant Natural := Get_Id (Command);
            Name_Image : constant String  := Column (Command, 2);
            Desc_Image : constant String  := Column (Command, 3);
         begin
            Ada.Text_IO.Put (Id'Img);
            Ada.Text_IO.Put ("  ");
            Ada.Text_IO.Put (Name_Image);
            Ada.Text_IO.Put ("  ");
            Ada.Text_IO.Put (Desc_Image);
            Ada.Text_IO.New_Line;
         end;
      end loop;
   end Put_Lists;

   procedure Add_Job (Id    : in Job_Id;
                      Title : in String)
   is
      use SQLite;
      Command : constant Statement :=
        Prepare (Database.DB,
                 "INSERT INTO Job (Id, Title) " &
                   "VALUES (?,?)");
   begin
      Bind (Command, 1, Interfaces.Integer_64 (Id));
      Bind (Command, 2, Title);
      Command.Step;
   end Add_Job;

   procedure Add_Event (Id        : in Job_Id;
                        Event     : in String;
                        Timestamp : in Ada.Calendar.Time)
   is
      use SQLite;
      use GNAT.Calendar.Time_IO;
      Command_1 : constant Statement :=
        Prepare (Database.DB,
                 "SELECT MAX(Id) + 1 FROM Event");
      Command_2 : constant Statement :=
        Prepare (Database.DB,
                 "INSERT INTO Event (Id, Job, Stamp, Kind) " &
                   "VALUES (?,?,?,?)");
      Event_Id : Interfaces.Integer_64;
   begin
      Command_1.Step;
      Event_Id := Interfaces.Integer_64 (Get_Id (Command_1));
      Bind (Command_2, 1, Event_Id);
      Bind (Command_2, 2, Interfaces.Integer_64 (Id));
      Bind (Command_2, 4, Event);
      Bind (Command_2, 3, Image (Timestamp, ISO_Date & " %H:%M:%S"));
      Command_2.Step;
   end Add_Event;

   procedure Create_Job (Id    : in Job_Id;
                         Title : in String) is
   begin
      Add_Job   (Id, Title);
      Add_Event (Id, "Created", Ada.Calendar.Clock);
   end Create_Job;

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
      Id := Interfaces.Integer_64 (Get_Id (Command_1));

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

end Database;
