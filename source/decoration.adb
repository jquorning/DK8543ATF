--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Symbols;

package body Decoration is

   use Symbols;

   subtype Mark_String is String (1 .. 3);
   To_Mark : constant array (State_Type) of Mark_String :=
     (Fresh     => " - ",
      Executing => " + ",
      Done      => UTF8 (White_Star), --  ;'*',
      Omitted   => " o ",
      Deleted   => " X ");
--            if Database.Events.Is_Done (Desc.Id) then
--               Put (Symbols.UTF8 (Symbols.Black_Star));
--            else
--               Put (Symbols.UTF8 (Symbols.White_Star));
--            end if;

   Space : constant String := "   ";

   Left_Parenthesis : constant String :=
     UTF8 (Medium_Flattened_Left_Parenthesis_Ornament);

   Right_Parenthesis : constant String :=
     UTF8 (Medium_Flattened_Right_Parenthesis_Ornament);


   function Status_Image (Status : Status_Type)
                         return String
   is
      First : constant String :=
        (if Status.Partly then Left_Parenthesis  else Space);
      Last  : constant String :=
        (if Status.Partly then Right_Parenthesis else Space);
      Mark  : constant String :=
        To_Mark (Status.State);
   begin
      return First & Mark & Last;
   end Status_Image;


   function Title_Image (Title  : String;
                         Status : Status_Type)
                        return String
   is
      First : constant Character := (if Status.Partly then '(' else ' ');
      Last  : constant Character := (if Status.Partly then ')' else ' ');
   begin
      return First & Title & Last;
   end Title_Image;


   function Current_Image (Status : Status_Type)
                          return String
   is
      pragma Unreferenced (Status);
   begin
      return "???";
   end Current_Image;


end Decoration;

