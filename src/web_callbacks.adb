------------------------------------------------------------------------------
--                                  Kontoplot                               --
--                                                                          --
--                     Copyright (C) 2014, Jesper Quorning                  --
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

with AWS.MIME;
with AWS.Services.Web_Block.Registry;
with AWS.Templates;

with GNAT.Traceback.Symbolic;

with Parser;  --  Web_Help

package body Web_Callbacks is

   Web_Base : constant String := "../web/";
   Translations : AWS.Templates.Translate_Set;

   procedure Initialize is
   begin
      AWS.Templates.Insert
        (Translations,
         AWS.Templates.Assoc ("COMMAND_TABLE",
                              Parser.Web_Help));
   end Initialize;

   ----------
   -- Main --
   ----------

   function Main (Request : in AWS.Status.Data)
                 return AWS.Response.Data
   is
      use AWS.Services;
      use AWS;

      URI      : constant String          := Status.URI (Request);
      Filename : constant String          := URI (URI'First + 1 .. URI'Last);
      Set      : Templates.Translate_Set;
   begin
      if URI = "/add" then
         Templates.Insert (Set, Templates.Assoc ("OP", "ADD"));

         return Web_Block.Registry.Build
           (Key          => "/",
            Request      => Request,
            Translations => Set);

      elsif URI = "/sub" then
         Templates.Insert (Set, Templates.Assoc ("OP", "SUB"));
         return Web_Block.Registry.Build
           (Key          => "/",
            Request      => Request,
            Translations => Set);

--        elsif URI = "/" then
--           Ada.Text_IO.Put_Line ("Serving main page " & URI);
--           Templates.Insert (Set, Templates.Assoc ("OP", "SUB"));
--           return Web_Block.Registry.Build
--             (Key          => "/",
--              Request      => Request,
--              Translations => Set);

      elsif
        URI = "/stylesheets/print.css" or
        URI = "/stylesheets/main.css" or
        URI = "/stylesheets/boilerplate.css" or
        URI = "/css/rg.css" or
        URI = "/css/royal_greenland.css"
      then
         Ada.Text_IO.Put_Line ("Serving stylesheet " & URI);
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
         declare

         begin

            return AWS.Response.Build
              (MIME.Text_HTML,
               Message_Body => AWS.Templates.Parse (Web_Base & "main.thtml",
                                                    Translations));
         end;

--        elsif URI = "/import" then
--           Ada.Text_IO.Put_Line ("Serving Import " & URI);
--           --  Saldo_Plot.Import_CSV ("../netbank/poster-2.csv");
--           Saldoer.Import_CSV (1, "../netbank/poster-grundkonto-2012.csv");
--           Saldoer.Import_CSV (1, "../netbank/poster-grundkonto-2013.csv");
--           Saldoer.Import_CSV (1, "../netbank/poster-grundkonto-2014.csv");
--           Saldoer.Lav_Serienummer (Konto => 1);

--           Saldoer.Import_CSV (2, "../netbank/poster-boliglaan.csv");
--           Saldoer.Lav_Serienummer (Konto => 2);

--           Saldoer.Import_CSV (3, "../netbank/poster-prioitet-bl.csv");
--           Saldoer.Lav_Serienummer (Konto => 3);

--           Saldoer.Import_CSV (4, "../netbank/poster-grundejer-201X.csv");
--           Saldoer.Lav_Serienummer (Konto => 4);

--           return AWS.Response.Build
--             (MIME.Text_HTML,
--              Message_Body => "<html><body><h1>Import</html>");

      elsif URI = "/test" then
         --  Konti.Test;
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
