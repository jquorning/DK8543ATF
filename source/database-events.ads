--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Interfaces;

with Types;

package Database.Events is

   package US renames Ada.Strings.Unbounded;

   type Event_Kind is (Created, Startet, Stalled, Done,
                       Text, Deadline, Milestone);
   type Event_Id   is new Interfaces.Integer_64;

   procedure Add_Event (Job   : in     Types.Job_Id;
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

   function Get_Job_Events (Job : in Types.Job_Id)
                           return Event_Lists.Vector;


   function Is_Done (Job : in Types.Job_Id) return Boolean;
   --  Is last event in events for Job a DONE.

end Database.Events;
