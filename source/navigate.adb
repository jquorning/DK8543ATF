--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--
-----------------------------------------------------------------------------
--  Navigate body
--  Todo lists are hieracial.
--

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Database.Jobs;

package body Navigate is

   type Level_Index is new Positive;

   use type Types.Job_Id;

   package Path_Vectors is
      new Ada.Containers.Vectors (Level_Index,
                                  Types.Job_Id);
   Path : Path_Vectors.Vector;

   procedure Top is
   begin
      Path := Path_Vectors.Empty_Vector;
      Path_Vectors.Append (Path, Types.Job_Id'(0));
   end Top;


   procedure Parent is
   begin
      Path.Delete_Last;
   end Parent;


   function Path_Image return String is
      use Ada.Strings.Unbounded;
      A : Unbounded_String;
   begin
      for Level of Path loop
         Append (A, " >>" & Level'Img);
      end loop;
      return To_String (A);
   end Path_Image;


   procedure Build_Path (Job : in Types.Job_Id) is
      Iter : Types.Job_Id := Job;
      Info : Types.Job_Info;
   begin
      Ada.Text_IO.Put_Line ("Build_Path" & Job'Img);
      Path := Path_Vectors.Empty_Vector;
      while Iter /= 0 loop
         Path_Vectors.Prepend (Path, Iter);
         Info := Database.Jobs.Get_Job_Info (Iter);
         Iter := Info.Parent;
      end loop;
      Path_Vectors.Prepend (Path, 0);

   end Build_Path;


   function Current_Job return Types.Job_Id is
   begin
      return Path.Last_Element;
   end Current_Job;


   procedure Refresh_List is

      subtype S2 is String (1 .. 2);
      function To_S2 (A : Natural) return S2;

      function To_S2 (A : Natural) return S2 is
         package Natural_IO is new Ada.Text_IO.Integer_IO (Positive);
         Image : S2;
      begin
         Natural_IO.Default_Width := 2;
         Natural_IO.Put (Image, A);
         if Image (1) = ' ' then
            Image (1) := '0';
         end if;
         return Image;
      end To_S2;

      use Database.Jobs;
      Set : constant Types.Job_Sets.Vector := Get_Jobs (Top_Level);
   begin
      List.Set.Clear;
      List.Refs.Clear;

      List.Current := Get_Current_Job;

      --  Build references for top level.
      for Job in Set.First_Index .. Set.Last_Index loop
         List.Set.Append  (Set (Job));
         List.Refs.Append
           (Ref_Pair'("J" & To_S2 (Natural (Job)), 0));
         if List.Set (Job).Id = List.Current then
            declare
               Subset : constant Types.Job_Sets.Vector
                 := Get_Jobs (List.Current);
               subtype Index_Range is Types.Job_Index
                 range Subset.First_Index .. Subset.Last_Index;
               Refs   : Ref_Vectors.Vector;
            begin
               for Index in Index_Range loop
                  Refs.Append
                    (("T" & To_S2 (Integer (Index)), 1));
               end loop;
               List.Set .Append (Subset);
               List.Refs.Append (Refs);
            end;
         end if;
      end loop;
   end Refresh_List;


   procedure Lookup_Job (Text    : in     String;
                         Job     :    out Types.Job_Id;
                         Success :    out Boolean) is
   begin
      for Index in List.Set.First_Index .. List.Set.Last_Index loop
         if List.Refs (Index).Ref = Text then
            Job     := List.Set (Index).Id;
            Success := True;
            return;
         end if;
      end loop;
      Success := False;
   end Lookup_Job;


begin
   Top;
end Navigate;
