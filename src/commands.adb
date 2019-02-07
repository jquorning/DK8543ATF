--
--
--

with Ada.Calendar;

with Database.Events;
with Database.Jobs;
with Navigate;

package body Commands is


   procedure Create_Job (Job    : in Types.Job_Id;
                         Title  : in String;
                         Parent : in Types.Job_Id)
   is
      Id : Database.Events.Event_Id;
      pragma Unreferenced (Id);
   begin
      Database.Jobs.Add_Job (Job, Title, Parent, "jquorning");
      Database.Events.Add_Event (Job,  Ada.Calendar.Clock,
                                 Database.Events.Created, Id);
   end Create_Job;

   procedure Set_Current_Job (Job : in Types.Job_Id) is
   begin
      Database.Jobs.Set_Current_Job (Job);
      Navigate.List.Current := Job;
      Navigate.Refresh_List;
   end Set_Current_Job;

end Commands;
