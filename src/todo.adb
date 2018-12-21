--
--
--

with Database;
with Parser;

procedure Todo is
begin
   Database.Open;

   Database.Get_Current (Database.Jobs.Current,
                         Database.Lists.Current);

   Database.Get_Lists (Database.Lists);
   Database.Get_Jobs (Database.Jobs);

   Parser.Put_Banner;
   Database.Put_Lists (Database.Lists);
   loop
      Parser.Put_Prompt;
      Parser.Parse_Input (Parser.Get_Input);
      exit when Parser.Exit_Program;
   end loop;

end Todo;
