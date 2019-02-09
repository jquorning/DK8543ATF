--
--  TODO
--  Decoration spec
--

with Status;

package Decoration is

   use Status;

   function Status_Image (Status : Status_Type)
                         return String;
   --  Return string descriping Status of a job.


   function Title_Image (Title  : String;
                         Status : Status_Type)
                        return String;
   --  Return Title with optional decoration depending on Status.


   function Current_Image (Status : Status_Type)
                          return String;
   --  Return marking when current.


end Decoration;

