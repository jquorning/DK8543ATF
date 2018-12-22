--
--
--

with Ada.Calendar;

with Database.Events;

package body Commands is

   procedure Create_Job (Job    : in Database.Job_Id;
                         Title  : in String;
                         Parent : in Database.Job_Id)
   is
      Id : Database.Events.Event_Id;
      pragma Unreferenced (Id);
   begin
      Database.Add_Job   (Job, Title, Parent, "jquorning");
      Database.Events.Add_Event (Job,  Ada.Calendar.Clock,
                                 Database.Events.Created, Id);
   end Create_Job;


--     procedure Show_List (List : in Database.List_Id) is
--     begin
--        Database.Get_Jobs (Database.Jobs, List => List);
--        Terminal_IO.Put_Jobs (Database.Jobs);
--     end Show_List;


end Commands;
