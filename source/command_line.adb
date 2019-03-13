--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

package body Command_Line is


   procedure Parse (Config : in out Setup.Configuration)
   is
      pragma Unreferenced (Config);

      use Ada.Command_Line;
   begin
      if Argument_Count = 1 then
         if Argument (1) = "--version" then
            Put_Version;
            raise Terminate_Program;
         elsif Argument (1) = "--help" then
            Put_Usage;
            raise Terminate_Program;
         else
            Put_Usage;
            raise Terminate_Program;
         end if;
      else
         null;
         --  Normal program run
      end if;
   end Parse;


   procedure Put_Usage is
      use Ada.Text_IO;
   begin
      Put_Line ("The program " & Setup.Get_Program_Name & " manages your " &
                  "projects and activities via web page.");
      New_Line;
      Put_Line ("$ " & Setup.Get_Program_Name & " [--port=PORT] [--dir=DIR]");
      Put_Line ("$ " & Setup.Get_Program_Name & " --version");
      Put_Line ("$ " & Setup.Get_Program_Name & " --help");
      New_Line;
      Put_Line ("--help      - Show this usage text and exit");
      Put_Line ("--version   - Show program version and exit");
      Put_Line ("--port=PORT - Web server port (Default 8080)");
      Put_Line ("--dir=DIR   - Colletion directory (Default current dir)");
      New_Line;
      Put_Line ("safari http://localhost:8080 - Web server port");
      New_Line;
   end Put_Usage;

   procedure Put_Version is
      use Setup, Ada.Text_IO;
   begin
      Put (Get_Program_Name & " (" & Get_Program_Version & ")");
      Put ("Build  (" & Get_Build_ISO8601_UTC & ")");
   end Put_Version;

end Command_Line;
