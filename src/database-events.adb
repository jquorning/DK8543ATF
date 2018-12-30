--
--  Database.Events body
--

with GNAT.Calendar.Time_IO;

with SQLite;

package body Database.Events is

   SQL_Add_Event_S : constant String := "SELECT MAX(Id) + 1 FROM Event";
   SQL_Add_Event_I : constant String
     := "INSERT INTO Event (Id, Job, Stamp, Kind) VALUES (?,?,?,?)";
   SQL_Is_Done_S : constant String
     := "SELECT Stamp, Kind FROM Event WHERE Job=? ORDER BY Stamp DESC";
   SQL_Get_Events_S : constant String
     := "SELECT Stamp, Kind FROM Event WHERE Job=? ORDER BY Stamp ASC";

   procedure Add_Event (Job   : in     Types.Job_Id;
                        Stamp : in     Ada.Calendar.Time;
                        Kind  : in     Event_Kind;
                        Id    :    out Event_Id)
   is
      use SQLite, Interfaces;
      use GNAT.Calendar.Time_IO;
      Command_1 : constant Statement := Database.DB.Prepare (SQL_Add_Event_S);
      Command_2 : constant Statement := Database.DB.Prepare (SQL_Add_Event_I);
   begin
      Command_1.Step;
      Id := Event_Id (Integer_64'(Command_1.Column (1)));

      Command_2.Bind (1, Integer_64 (Id));
      Command_2.Bind (2, Integer_64 (Job));
      Command_2.Bind (3, Image (Stamp, ISO_Date & " %H:%M:%S"));
      Command_2.Bind (4, Event_Kind'Image (Kind));
      Command_2.Step;
   end Add_Event;


   function Get_Job_Events (Job : in Types.Job_Id)
                           return Event_Lists.Vector
   is
      use SQLite, Interfaces;
      use Ada.Strings.Unbounded;

      Command : constant Statement := Database.DB.Prepare (SQL_Get_Events_S);
      Events  : Event_Lists.Vector;
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


   function Is_Done (Job : in Types.Job_Id)
                    return Boolean
   is
      use SQLite;

      Command : constant Statement := Database.DB.Prepare (SQL_Is_Done_S);
   begin
      Command.Bind (1, Interfaces.Integer_64 (Job));

      if Command.Step then
         declare
--            Stamp_Image : constant String := Command.Column (1);
            Kind_Image  : constant String := Command.Column (2);
         begin
            return Kind_Image = "DONE";
         end;
      end if;

      return False;  --  No events for Job

   end Is_Done;


end Database.Events;
