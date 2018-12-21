--
--
--

with Database;

package Terminal_IO is

   procedure Put_Jobs (Jobs : in Database.Job_Set);
   procedure Put_Lists (Lists : in Database.List_Set);

end Terminal_IO;
