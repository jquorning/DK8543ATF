--
--
--

with Database.Events;
with Ada.Calendar;

package body Commands is

   procedure Create_Job (Job   : in Database.Job_Id;
                         Title : in String;
                         List  : in Database.List_Id) is
      Id : Database.Events.Event_Id;
      pragma Unreferenced (Id);
   begin
      Database.Add_Job   (Job, Title, List, "jquorning");
      Database.Events.Add_Event (Job,  Ada.Calendar.Clock,
                                 Database.Events.Created, Id);
   end Create_Job;

end Commands;
