--
--
--

with Ada.Calendar;
with Ada.Containers.Vectors;

with Interfaces;

package Database.Events is

   type Event_Kind is (Created, Startet, Stalled, Done,
                       Text, Deadline, Milestone);
   type Event_Id   is new Interfaces.Integer_64;

   procedure Add_Event (Job   : in     Job_Id;
                        Stamp : in     Ada.Calendar.Time;
                        Kind  : in     Event_Kind;
                        Id    :    out Event_Id);

   type Event_Info is
      record
         Stamp : US.Unbounded_String;
         Kind  : US.Unbounded_String;
      end record;

   package Event_Lists is
      new Ada.Containers.Vectors (Positive, Event_Info);

   function Get_Job_Events (Job : in Job_Id)
                           return Event_Lists.Vector;


   function Is_Done (Job : in Job_Id) return Boolean;
   --  Is last event in events for Job a DONE.

end Database.Events;
