--
--
--

with Database;

package Terminal_IO is

   procedure Put_Jobs (Jobs : in Database.Job_Set);
   --   procedure Put_Lists (Lists : in Database.List_Set);
   procedure Show_Job (Job : in Database.Job_Id);
   procedure Put_Help;
   procedure Put_Error (Text : in String);
   procedure Put_Banner;

end Terminal_IO;
