--
--
--

with Database;

package Web_IO is

   function Jobs_Image (Jobs : in Database.Job_Set)
                       return String;

   function Lists_Image (Lists : in Database.List_Set)
                        return String;

end Web_IO;
