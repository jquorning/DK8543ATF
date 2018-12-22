--
--
--

with Ada.Text_IO;

with Database;
with Parser;
with Web_Server;
with Terminal_IO;
with Interactive;

procedure Todo is

   procedure Show (Top : Database.Job_Id);

   procedure Show (Top : Database.Job_Id) is
      Jobs : Database.Job_Set;
   begin
      Database.Get_Jobs (Jobs, Top);
      Terminal_IO.Put_Jobs (Jobs);
   end Show;

begin
   Interactive.Initialize;
   Web_Server.Startup;
   Database.Open;

   Terminal_IO.Put_Banner;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Top Jobs:");
   Show (Database.Top_Level);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Current Jobs:");
   Show (Database.Get_Current_Job);

   loop
      Parser.Parse_Input (Interactive.Get_Line);
      exit when Parser.Exit_Program;
   end loop;

   Web_Server.Shutdown;
   Interactive.Finalize;
end Todo;
