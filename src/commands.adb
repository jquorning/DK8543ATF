--
--
--

with Ada.Calendar;

with Database.Events;

package body Commands is


   procedure Create_Job (Job    : in Database.Jobs.Job_Id;
                         Title  : in String;
                         Parent : in Database.Jobs.Job_Id)
   is
      Id : Database.Events.Event_Id;
      pragma Unreferenced (Id);
   begin
      Database.Jobs.Add_Job (Job, Title, Parent, "jquorning");
      Database.Events.Add_Event (Job,  Ada.Calendar.Clock,
                                 Database.Events.Created, Id);
   end Create_Job;


end Commands;
