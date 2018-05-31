--
--
--

with Database;

package Commands is

   procedure Create_Job (Job   : in Database.Job_Id;
                         Title : in String;
                         List  : in Database.List_Id);

end Commands;
