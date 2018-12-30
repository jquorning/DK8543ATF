--
--
--

with Database.Jobs;

package Web_IO is

   subtype HTML_String is String;

   function Help_Image return HTML_String;

   function Jobs_Image (Jobs : in Database.Jobs.Job_Set)
                       return HTML_String;

   function Job_Image (Job : in Database.Jobs.Job_Id)
                      return HTML_String;

end Web_IO;
