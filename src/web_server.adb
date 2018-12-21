--
--  This is a very simple Web Page Server using the AWS.Services.Page_Server.
--

with Ada.Text_IO;

with AWS.Config;
with AWS.Server.Log;
with AWS.Services.Page_Server;
--  with AWS.Services.Web_Block.Registry;
--  with AWS.Templates;

with Web_Callbacks;

package body Web_Server is

   Server       : AWS.Server.HTTP;

   procedure Startup is

      Config : constant AWS.Config.Object := AWS.Config.Get_Current;

   begin
--      Database.Open;
      Ada.Text_IO.Put_Line ("AWS " & AWS.Version);
      Ada.Text_IO.Put_Line
        ("Server port:" & Integer'Image (AWS.Config.Server_Port (Config)));

      if AWS.Config.Directory_Browser_Page (Config) /= "" then
         AWS.Services.Page_Server.Directory_Browsing (True);
      end if;

      if AWS.Config.Log_Filename_Prefix (Config) /= "" then
         AWS.Server.Log.Start (Server);
      end if;

      if AWS.Config.Error_Log_Filename_Prefix (Config) /= "" then
         AWS.Server.Log.Start_Error (Server);
      end if;

--      AWS.Services.Web_Block.Registry.Register
--        ("/", "../web/main.thtml", null);

--           Templates.Insert
--             (Translations,
--              Templates.Assoc ("TIL_DATO",
--                               My_Dates.To_ISO8601_Date (Til_Dato)));
      Web_Callbacks.Initialize;
      AWS.Server.Start (Server, "web_block", Web_Callbacks.Main'Access);

   end Startup;


   procedure Shutdown is
   begin
      Ada.Text_IO.Put_Line ("AWS server shutdown in progress...");
      AWS.Server.Shutdown (Server);
   end Shutdown;


end Web_Server;

