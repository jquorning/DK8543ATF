--
--
--

package Setup is

   Program_Version    : constant String := "2019-01-14";
   Program_Name       : constant String := "to-do-it";
   Database_Extension : constant String := ".todoit";

   Default_Database   : constant String :=
     Program_Name & Database_Extension;

   type Configuration is record
      null;
   end record;

end Setup;
