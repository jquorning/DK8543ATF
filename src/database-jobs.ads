--
--  Database
--

with Types;

package Database.Jobs is

   function Get_Current_Job return Types.Job_Id;
   --  Get current job from persistent database storage.

   procedure Set_Current_Job (Job : in Types.Job_Id);
   --  Set current job in persistent storage.

   use type Types.Job_Id;
   Top_Level : constant Types.Job_Id := 0;
   All_Jobs  : constant Types.Job_Id := -1;

   function Get_Jobs (Top : in Types.Job_Id)
                     return Types.Job_Sets.Vector;
   --  Get set of jobs at level Top.

   function Get_Job_Info (Job : in Types.Job_Id)
                         return Types.Job_Info;


   procedure Add_Job (Id     : in Types.Job_Id;
                      Title  : in String;
                      Parent : in Types.Job_Id;
                      Owner  : in String);

   function Get_New_Job_Id return Types.Job_Id;
   --  Get an Job_Id for a new job.


   procedure Transfer (Job       : in Types.Job_Id;
                       To_Parent : in Types.Job_Id);


end Database.Jobs;
