pragma License (Restricted);
--
--  Copyright (C) 2020 Jesper Quorning All Rights Reserved.
--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

with Setup;
with Command_Line;
with Database.Jobs;
with SQL_Database;
with Parser;
with Web_Server;
with Terminal_IO;
with Interactive;
with Navigate;
with Files;

procedure iDoNu_Program
is
   Config : Setup.Configuration;
begin
   declare
      List : Files.Collection_List;
   begin
      Files.Append_Collections (List, ".");
      Files.Append_Collections (List, "..");
      Files.Append_Collections (List, "/Users/jquorning/");
      Files.Append_Collections (List, "/Users/jquorning/var/");
   end;

   Command_Line.Parse (Config);
   Interactive.Initialize;
   Web_Server.Startup;
   SQL_Database.Open;

   Terminal_IO.Put_Banner;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("List:");
   Navigate.Build_Path (Database.Jobs.Get_Current_Job);
   Terminal_IO.Put_Path;
   Navigate.Refresh_List;
   Terminal_IO.Put_Jobs;

   loop
      Parser.Parse_Input (Interactive.Get_Line);
      exit when Parser.Exit_Program;
   end loop;

   Web_Server.Shutdown;
   Interactive.Finalize;

   Command_Line.Set_Exit_Status (Command_Line.Success);

exception

   when Command_Line.Terminate_Program =>
      null;

end iDoNu_Program;
