--
--  Navigate
--  Todo lists are hieracial.

with Ada.Containers.Vectors;

with Types;

package Navigate is

   procedure Top;
   --  Go to top level.

   procedure Parent;
   --  Go one level up. If top level: stay there.

   function Path_Image return String;
   --  Image of path.

   procedure Build_Path (Job : in Types.Job_Id);
   --  Build Path from Job and up.

   function Current_Job return Types.Job_Id;
   --  Get current job.

   procedure Refresh_List;
   --  Refresh job naviagtion list.

   procedure Lookup_Job (Text    : in     String;
                         Job     :    out Types.Job_Id;
                         Success :    out Boolean);
   --  Loopup Text as job in navigation list. Job is set on Success.


   subtype Ref_Type is String (1 .. 3);
   type Ref_Pair is
      record
         Ref   : Ref_Type;
         Level : Natural;  --  0: Top level
      end record;

   package Ref_Vectors is
      new Ada.Containers.Vectors (Types.Job_Index, Ref_Pair);

   type List_Type is
      record
         Set     : Types.Job_Sets.Vector;
         Refs    : Ref_Vectors.Vector;
         Current : Types.Job_Id;
      end record;

   List : List_Type;


end Navigate;
