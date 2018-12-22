--
--
--

package body Readline_Helper is

   type String_Access is access constant String;

   type Compl_List is array (Natural range <>) of String_Access;
   --  List of commands to match

   function Base_Commands (Text : String; State : Integer) return String;
   --  Return todo commands one by one iterating by State from 0 and up.

   function Null_Commands (Text : String; State : Integer) return String;
   --  Return NUL string unconditionally signalling no completions.

   function Find (Text  : String;
                  State : Integer;
                  List  : Compl_List) return String;
   --  Find Text in List. Is to be called with State iterating
   --  from 0 and up. Returns matching string from List or "" when
   --  no more entrys matched.

   function "-" (S : String) return String_Access;
   --  Helper.


   function "-" (S : String) return String_Access is
   begin
      return new String'(S);
   end "-";


   function Find (Text  : String;
                  State : Integer;
                  List  : Compl_List) return String
   is
      Count : Integer := 0;
   begin
      for Ent of List loop
         declare
            ST : constant String := Ent.all;
         begin
            if Text'Length <= ST'Length and then Text = ST (Text'Range) then
               if State = Count then
                  return ST;
               end if;
               Count := Count + 1;
            end if;
         end;
      end loop;
      return "";
   end Find;


   Command_List : constant Compl_List :=
     (-"view", -"lists", -"set", -"add",
      -"move", -"trans", -"help", -"quit",
      -"show", -"event");


   function Base_Commands (Text : String; State : Integer) return String is
      Match : constant String := Find (Text, State, Command_List);
   begin
      return Match;
   end Base_Commands;

   function Null_Commands (Text : String; State : Integer) return String
   is
      pragma Unreferenced (Text, State);
   begin
      return "";
   end Null_Commands;


--     function Entry_Func (Text : String; State : Integer) return String;
--     function Entry_Func (Text : String; State : Integer) return String is
--     begin
--        case Command is
--           when Base   =>  return Find (Text, State, BAS);
--           when Loop_1 =>  return Find (Text, State, CMD1);
--           when Loop_2 =>  return Find (Text, State, CMD2);
--           when Define =>  return Find (Text, State, KEY);
--           when T_Procedure =>  null;
--              --  return Symbols.Find (Symbols.K_Procedure,
--              --          Text, State);
--           when Load   =>  return "";
--           when Save   =>  return "";
--           when others =>  return "";
--        end case;
--     end Entry_Func;

--     procedure Parse (S : String);
--     procedure Parse (S : String) is
--        use Ada.Strings.Fixed;
--        Pos : constant Natural := Index (S, " ");
--        CMD : constant String  := S (S'First .. Pos - 1);
--  --      LC  : constant String  := To_Lower (CMD);
--     begin
--        if    CMD = ""      then  Command := Base;
--        elsif CMD = ".save" then  Command := Save;
--        elsif CMD = ".load" then  Command := Load;
--        elsif CMD = ".def"  then  Command := Define;
--        elsif CMD = "loop1" then  Command := Loop_1;
--        elsif CMD = "loop2" then  Command := Loop_2;
--        elsif CMD = "procedure" then  Command := T_Procedure;
--        else                      Command := None;
--        end if;
--     end Parse;


   function Completer
     (Full_Line   : String;
      Text        : String;
      Start, Last : Integer)
     return GNATCOLL.Readline.Possible_Completions
   is
      pragma Unreferenced (Full_Line, Last);
   begin
--      Parse (Full_Line);
      if Start = 0 then
         return
           GNATCOLL.Readline.Completion_Matches (Text, Base_Commands'Access);
      end if;
      return
        GNATCOLL.Readline.Completion_Matches (Text, Null_Commands'Access);
   end Completer;

end Readline_Helper;
