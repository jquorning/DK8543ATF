--
--  Database
--

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with Interfaces;

package Database.Jobs is

   package US renames Ada.Strings.Unbounded;

   type Job_Id is new Interfaces.Integer_64;

   type Job_Desc is
      record
         Id    : Job_Id;
         Title : US.Unbounded_String;
      end record;

   type Job_Index is new Positive;
   package Job_Sets is
      new Ada.Containers.Vectors (Job_Index, Job_Desc);

   function Get_Current_Job return Job_Id;
   --  Get current job from persistent database storage.

   procedure Set_Current_Job (Job : in Job_Id);
   --  Set current job in persistent storage.

   procedure Open;

   Top_Level : constant Job_Id := 0;
   All_Jobs  : constant Job_Id := -1;

   function Get_Jobs (Top : in Job_Id)
                     return Job_Sets.Vector;
   --  Get set of jobs at level Top.

   type Job_Info is
      record
         Title  : US.Unbounded_String;
         Parent : Job_Id;
         Owner  : US.Unbounded_String;
      end record;

   function Get_Job_Info (Job : in Job_Id) return Job_Info;


   procedure Add_Job (Id     : in Job_Id;
                      Title  : in String;
                      Parent : in Job_Id;
                      Owner  : in String);

   function Get_New_Job_Id return Job_Id;
   --  Get an Job_Id for a new job.


   procedure Transfer (Job       : in Job_Id;
                       To_Parent : in Job_Id);


end Database.Jobs;
