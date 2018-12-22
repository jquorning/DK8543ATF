--
--
--

with Database;

package Web_IO is

   subtype HTML_String is String;

   function Help_Image return HTML_String;

   function Jobs_Image (Jobs : in Database.Job_Set)
                       return HTML_String;

--   function Lists_Image (Lists : in Database.List_Set)
--                        return HTML_String;

   function Job_Image (Job : in Database.Job_Id)
                      return HTML_String;

end Web_IO;
