------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2018, Jesper Quorning                  --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with AWS.MIME;
with AWS.Templates;
with AWS.Parameters;

with GNAT.Traceback.Symbolic;

with Parser;  --  Web_Help
with Database;
with Web_IO;

package body Web_Callbacks is

   Web_Base : constant String := "../web/";
   Translations : AWS.Templates.Translate_Set;

   procedure Associate (Placeholder : String;
                        Value       : String);
   --  Update template translation Placeholder with Value.

   procedure Associate (Placeholder : String;
                        Value       : String)
   is
   begin
      AWS.Templates.Insert (Translations,
                            AWS.Templates.Assoc (Placeholder, Value));
   end Associate;

   procedure Initialize is
   begin
      --  Static translations
      Associate ("COMMAND_TABLE", Parser.Web_Help);
   end Initialize;

   function Current_List_Name (Lists : Database.List_Set) return String;
   --  Return name of current list

   function Current_List_Name (Lists : Database.List_Set) return String is
      use type Database.List_Id;
   begin
      for List of Lists.Vector loop
         if Lists.Current = List.Id then
            return Ada.Strings.Unbounded.To_String (List.Name);
         end if;
      end loop;
      return "UNKNOWN=XXX";
   end Current_List_Name;

   procedure Serve_Main_Page (Request : in AWS.Status.Data);
   --  Build main web page "/"

   procedure Serve_Main_Page (Request : in AWS.Status.Data) is
      List : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      CMD  : constant String := AWS.Parameters.Get (List, "cmd");
   begin
      Parser.Parse_Input (CMD);

      Database.Get_Jobs (Database.Jobs);
      Database.Get_Lists (Database.Lists);

      Associate ("CURRENT_LIST", Current_List_Name (Database.Lists));
      Associate ("LISTS_TABLE",  Web_IO.Lists_Image (Database.Lists));
      Associate ("JOBS_TABLE",   Web_IO.Jobs_Image (Database.Jobs));
      Associate ("LAST_COMMAND", Parser.Get_Last_Command);
   end Serve_Main_Page;

   ----------
   -- Main --
   ----------

   function Main (Request : in AWS.Status.Data)
                 return AWS.Response.Data
   is
      use AWS;

      URI      : constant String          := Status.URI (Request);
      Filename : constant String          := URI (URI'First + 1 .. URI'Last);
   begin

      if
        URI = "/stylesheets/print.css" or
        URI = "/stylesheets/main.css" or
        URI = "/stylesheets/boilerplate.css" or
        URI = "/css/rg.css" or
        URI = "/css/royal_greenland.css"
      then
         return AWS.Response.Build
           (MIME.Text_CSS,
            Message_Body => Templates.Parse (Web_Base & Filename));

--        elsif
--          URI = "/js/flot/jquery.min.js" or
--          URI = "/js/flot/jquery.flot.min.js" or
--          URI = "/js/flot/jquery.flot.crosshair.min.js" or
--          URI = "/js/flot/jquery.flot.symbol.js" or
--          URI = "/js/flot/jquery.flot.symbol.js.jq" or
--          URI = "/js/flot/jquery.flot.time.js"
--        then
--           Ada.Text_IO.Put_Line ("Serving flot " & URI);
--           return AWS.Response.Build
--             (MIME.Text_HTML,
--              Message_Body => Templates.Parse (Web_Base & Filename));

      elsif URI = "/favicon.ico" then
         Ada.Text_IO.Put_Line ("Serving ikon " & URI);
         return AWS.Response.Build
           (MIME.Text_HTML, Message_Body
              => Templates.Parse (Web_Base & "favicon.ico"));

      elsif URI = "/" then
         Serve_Main_Page (Request);
         return AWS.Response.Build
           (MIME.Text_HTML,
            Message_Body => AWS.Templates.Parse (Web_Base & "main.thtml",
                                                 Translations));

      elsif URI = "/test" then
         return AWS.Response.Build
           (MIME.Text_HTML,
            Message_Body => "<html><head><title>Test</title></head>" &
              "<body><h1>Test</html>");

      else
         Ada.Text_IO.Put_Line ("URI is " & URI);
         Ada.Text_IO.Put_Line ("Filename is " & Filename);
         return AWS.Response.Build
           (MIME.Text_HTML,
            Message_Body => Templates.Parse (Web_Base & "fejl.html"));
      end if;

   exception

      when others =>
         declare --  Call_Stack
            Trace  : GNAT.Traceback.Tracebacks_Array (1 .. 100);
            Length : Natural;
         begin
            GNAT.Traceback.Call_Chain (Trace, Length);
            Ada.Text_IO.Put_Line
              (GNAT.Traceback.Symbolic.Symbolic_Traceback
                 (Trace (1 .. Length)));
         end;
         raise;

   end Main;


end Web_Callbacks;
