--
--  Database.Events body
--

with GNAT.Calendar.Time_IO;

with SQLite;

package body Database.Events is

   procedure Add_Event (Job   : in     Job_Id;
                        Stamp : in     Ada.Calendar.Time;
                        Kind  : in     Event_Kind;
                        Id    :    out Event_Id)
   is
      use SQLite, Interfaces;
      use GNAT.Calendar.Time_IO;
      Command_1 : constant Statement :=
        Prepare (Database.DB,
                 "SELECT MAX(Id) + 1 FROM Event");
      Command_2 : constant Statement :=
        Prepare (Database.DB,
                 "INSERT INTO Event (Id, Job, Stamp, Kind) " &
                   "VALUES (?,?,?,?)");
   begin
      Command_1.Step;
      Id := Event_Id (Integer_64'(Command_1.Column (1)));

      Command_2.Bind (1, Integer_64 (Id));
      Command_2.Bind (2, Integer_64 (Job));
      Command_2.Bind (3, Image (Stamp, ISO_Date & " %H:%M:%S"));
      Command_2.Bind (4, Event_Kind'Image (Kind));
      Command_2.Step;
   end Add_Event;


   function Is_Done (Job : in Job_Id)
                    return Boolean
   is
      use SQLite;

      Command : constant Statement :=
        Prepare (Database.DB,
                 "SELECT Stamp, Kind FROM Event " &
                   "WHERE Job=? ORDER BY Stamp DESC");
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
