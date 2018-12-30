--
--
--

with Types;

package Web_IO is

   subtype HTML_String is String;

   function Help_Image return HTML_String;

   function Jobs_Image (Jobs : in Types.Job_Sets.Vector)
                       return HTML_String;

   function Job_Image (Job : in Types.Job_Id)
                      return HTML_String;

end Web_IO;
