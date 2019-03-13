--
--
--  Files spec
--

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

package Files is

   use Ada.Strings.Unbounded;

   package Collection_Lists is
      new Ada.Containers.Doubly_Linked_Lists
     (Ada.Strings.Unbounded.Unbounded_String);

   subtype Collection_List is Collection_Lists.List;

   procedure Append_Collections (Collection : in out Collection_List;
                                 Directory  : in     String);
   --  Scan Directory adding valid databases to Collection.

end Files;

