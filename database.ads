--
--
--

package Database is

   type Job_Id is new Natural;

   procedure Open;

   procedure Put_Jobs;

   procedure Put_Lists;

   procedure Create_Job (Id    : in Job_Id;
                         Title : in String);

   procedure Create_List (Name : in String);

   function Get_Job_Id return Job_Id;

end Database;
