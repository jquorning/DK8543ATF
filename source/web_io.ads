--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Types;

package Web_IO is

   subtype HTML_String is String;

   function Help_Image return HTML_String;

   function Jobs_Image return HTML_String;

   function Job_Image (Job : in Types.Job_Id)
                      return HTML_String;

end Web_IO;
