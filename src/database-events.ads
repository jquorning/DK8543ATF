--
--
--

with Ada.Calendar;
with Interfaces;

package Database.Events is

   type Event_Kind is (Created, Startet, Stalled, Completed,
                       Commented, Deadline, Milestone);
   type Event_Id   is new Interfaces.Integer_64;

   procedure Add_Event (Job   : in     Job_Id;
                        Stamp : in     Ada.Calendar.Time;
                        Kind  : in     Event_Kind;
                        Id    :    out Event_Id);

end Database.Events;
