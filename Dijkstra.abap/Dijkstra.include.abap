*&---------------------------------------------------------------------*
*& Include          ZINCLDIJK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*& HINTS:
*&  - include the local classes in your own reports, or import as
*&    global classes
*&  - usage: see unit tests (lct_...  classes)
*&---------------------------------------------------------------------*
"-----------------------------------------------------------------------------------------------------------
" Interface: lif_graph
" the graph interface makes it possible to have several implementation of the graph
"-----------------------------------------------------------------------------------------------------------
interface lif_graph.
types:
  begin of t_pair_of_name_and_dist,
    name type string,
    dist type int8,
  end   of t_pair_of_name_and_dist,
  tt_pair_of_name_and_dist type hashed table of t_pair_of_name_and_dist with unique key name,
  tt_node_list type standard table of string with default key.

methods:
  add_node importing name type string,
  add_neighbour importing value(name) type string value(neighbour_name) type string value(distance) type int8,
  get_nodes returning value(it_nodes) type tt_node_list,
  get_neighbours importing value(name) type string returning value(it_neighbours) type  tt_pair_of_name_and_dist.
endinterface.

"-----------------------------------------------------------------------------------------------------------
" lcl_adjacency_list : lif_graph
" implentation of the graph interface as adjacency list
"-----------------------------------------------------------------------------------------------------------
class lcl_adjacency_list definition.
public section.
  types: begin of t_adjacence,
          name type string,
          neighbours type lif_graph=>tt_pair_of_name_and_dist,
         end   of t_adjacence,
         tt_adjacence type standard table of t_adjacence with default key.

  interfaces lif_graph.

  methods:
    constructor importing value(it_graph) type tt_adjacence optional. " ??? how can i have multiple constructors ??
private section.
  methods get_node_adjacence importing name type string returning value(r_node) type ref to t_adjacence.
  methods is_in_graph importing name type string returning value(b) type ABAP_BOOL.
  data it_adjacence type  tt_adjacence.
endclass.

class lcl_adjacency_list implementation.
  method constructor.
    if it_graph is supplied.
      append lines of it_graph to me->it_adjacence.
    endif.
  endmethod.
  method get_node_adjacence.
    read table it_adjacence
      with key name = name
      reference into r_node.
  endmethod.

  method is_in_graph.
    data(r_node) = get_node_adjacence( name ).
    if r_node is not bound.
      b = abap_false.
    else.
      b = abap_true.
    endif.
  endmethod.

  method lif_graph~add_node.
    " TODO use hash tables
    if not is_in_graph( name ).
      append value #( name = name ) to it_adjacence.
    endif.
  endmethod.

  method lif_graph~add_neighbour.
    data(r_node) = get_node_adjacence( name ).
    if r_node is not bound.
      " TODO throw exception.
    else.
      insert value #( name = neighbour_name dist = distance ) into table r_node->*-neighbours.
    endif.
  endmethod.

  method lif_graph~get_nodes.
    it_nodes = value lif_graph=>tt_node_list( for line in it_adjacence ( line-name ) ).
  endmethod.

  method lif_graph~get_neighbours.
    data(r_node) = get_node_adjacence( name ).
    it_neighbours = r_node->*-neighbours.
  endmethod.
endclass.

"-----------------------------------------------------------------------------------------------------------
" Unit Test
"-----------------------------------------------------------------------------------------------------------
class ltc_adjacency_list definition for testing risk level harmless duration short.
  private section.
  methods:
    setup,
    teardown,
    test_add_node for testing.
endclass.
class ltc_adjacency_list implementation.
    method setup.
    endmethod.

    method teardown.
    endmethod.

    method test_add_node.
      data r_graph type ref to lif_graph.
      r_graph = new lcl_adjacency_list( ).
      r_graph->add_node( '1' ).
      r_graph->add_neighbour( name = '1'  neighbour_name = '2' distance = 1 ).
      r_graph->add_neighbour( name = '1'  neighbour_name = '4' distance = 3 ).
      r_graph->add_node( '2' ).
      r_graph->add_neighbour( name = '2'  neighbour_name = '3' distance = 1 ).
      r_graph->add_node( '3' ).
      r_graph->add_node( '4' ).
      r_graph->add_neighbour( name = '4'  neighbour_name = '3' distance = 2 ).

      cl_abap_unit_assert=>assert_equals(
        act = r_graph->get_nodes( )
        exp = value lif_graph=>tt_node_list( ( `1` ) ( `2` ) ( `3` ) ( `4` ) )
      ).
    endmethod.
endclass.


"-----------------------------------------------------------------------------------------------------------
" The Dijkstra Alg
"   see: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
" Usage:
"         use constructor to parametrise the alg with the graph and start node
"         use public method run to run the alg, run returns the list of node pairs, first node in the pair
"             is the destination node, second one is the predecessor on the way to the start node.
"             to find a way to a designated node, search the node in the resulting list, go back to start,
"             remember nodes you visited, reverse the order
"-----------------------------------------------------------------------------------------------------------
class lcl_dijkstra definition.
public section.
  types: begin of t_pred,
           nd type string,
           pred type string,
         end of t_pred,
         tt_pred type hashed table of t_pred with unique key primary_key components nd.
  methods:
    constructor importing value(r_graph) type ref to lif_graph,
    run         importing value(start_node_name) type string exporting it_path type tt_pred.

private section.
  types: begin of t_dist,
           nd type string,
           dist type int8,
         end of t_dist,
         tt_dist type hashed table of t_dist with unique key primary_key components nd
                                             with non-unique sorted key dist components dist,  " mimics priority queue
         tt_queue type hashed table of string with unique default key.

  class-data: infinity type int8 value cl_abap_math=>max_int8.

  data: r_graph type ref to lif_graph,
        it_dist type tt_dist,
        it_pred type tt_pred,
        it_queue type tt_queue,
        start type string.
  methods:
        init_alg     importing value(start) type string,
        update_dist  importing value(u) type string value(v) type string,
        get_min_dist_node returning value(min_dist_node) type string.

endclass.

class lcl_dijkstra implementation.
" public
  method constructor.
    me->r_graph = r_graph.
    clear start.
    free it_queue.
    free it_pred.
    free it_dist.
  endmethod.

  method run.
    init_alg( start_node_name ).

    while lines( it_queue ) > 0.
      data(u) = get_min_dist_node( ).
      delete it_queue where table_line = u.
      data(it_neighbours) = r_graph->get_neighbours( u ).
      loop at it_neighbours assigning field-symbol(<fs_v>).
        if line_exists( it_queue[ table_line = <fs_v>-name ] ).
          update_dist( u = u v = <fs_v>-name ).
        endif.
      endloop.
    endwhile.
    it_path = it_pred. " TODO - fill in dinstances and provide function to extract path to a single destination
  endmethod.

" private
  method init_alg.
    me->start = start.
    free it_queue.
    free it_pred.
    free it_dist.

    it_queue = r_graph->get_nodes( ).
    loop at it_queue assigning field-symbol(<fs_node>).
        insert value #( nd = <fs_node> dist = lcl_dijkstra=>infinity ) into table it_dist.
        insert value #( nd = <fs_node> pred = space ) into table it_pred.
            endloop.
    it_dist[ key primary_key components nd = start ]-dist = 0.
  endmethod.

  method update_dist.
    try.
      data(neighbours_of_u)      = r_graph->get_neighbours( u ).
      data(dist_from_u_to_v)     = neighbours_of_u[ name = v ]-dist.
      data(dist_from_start_to_u) = it_dist[ key primary_key components nd = u ]-dist.
      data(new_distance) = dist_from_start_to_u + dist_from_u_to_v.
      if new_distance < it_dist[ key primary_key components nd = v ]-dist.
        it_dist[ key primary_key components nd = v ]-dist = new_distance.
        it_pred[ nd = v ]-pred = u.
      endif.
    catch CX_SY_ARITHMETIC_OVERFLOW.
      " TODO in real world you have to distingish overflow from inifinite
    endtry.
  endmethod.

  method get_min_dist_node.
      " XXX this mimics a priority queue structure, maybe a dedicated implementation would be better
      "     try to encapsulate it into an interface and compare the performance of several implementations
      data(filtered) = filter #( it_dist in it_queue where nd = table_line ).
      READ TABLE filtered
                   assigning field-symbol(<fs>)
                   index 1 using key dist.
    min_dist_node = <fs>-nd.
  endmethod.

endclass.

"-----------------------------------------------------------------------------------------------------------
" Unit Test Dijkstra
"-----------------------------------------------------------------------------------------------------------
class lct_dijkstra definition for testing risk level harmless duration short.
  private section.
  data       r_graph type ref to lif_graph.
  methods:
             setup,
             teardown,
             test_run for testing.
endclass.

class lct_dijkstra implementation.
    method setup.
      " graph taken from the wikipedia animated example
      data(it_graph) = value lcl_adjacency_list=>tt_adjacence(
                                            ( name = `1` neighbours = value #(
                                                                                ( name = `6` dist = 14 )
                                                                                ( name = `3` dist = 09 )
                                                                                ( name = `2` dist = 07 )
                                                                             )
                                            )
                                            ( name = `2` neighbours = value #(
                                                                                ( name = `1` dist = 07 )
                                                                                ( name = `3` dist = 10 )
                                                                                ( name = `4` dist = 15 )
                                                                             )
                                            )
                                            ( name = `3` neighbours = value #(
                                                                                ( name = `1` dist = 09 )
                                                                                ( name = `6` dist = 02 )
                                                                                ( name = `4` dist = 11 )
                                                                                ( name = `2` dist = 10 )
                                                                             )
                                            )
                                            ( name = `4` neighbours = value #(
                                                                                ( name = `5` dist = 06 )
                                                                                ( name = `3` dist = 11 )
                                                                                ( name = `2` dist = 15 )
                                                                             )
                                            )
                                            ( name = `5` neighbours = value #(
                                                                                ( name = `6` dist = 9 )
                                                                                ( name = `4` dist = 6 )
                                                                             )
                                            )
                                            ( name = `6` neighbours = value #(
                                                                                ( name = `1` dist = 14 )
                                                                                ( name = `3` dist = 02 )
                                                                                ( name = `5` dist = 09 )
                                                                             )
                                            )
                                         ).
      r_graph = new lcl_adjacency_list( it_graph ).
    endmethod.

    method teardown.
      free  r_graph.
      clear r_graph.
    endmethod.

    method test_run.
      data(r_dijkstra) = new lcl_dijkstra( r_graph = r_graph ).
      data it_paths type lcl_dijkstra=>tt_pred.
      call method r_dijkstra->run exporting start_node_name = `1` importing it_path = it_paths.

      cl_abap_unit_assert=>assert_equals(
        act = it_paths
        exp = value lcl_dijkstra=>tt_pred( ( nd = `1` pred = space )
                             ( nd = `2` pred = `1` )
                             ( nd = `3` pred = `1` )
                             ( nd = `4` pred = `3` )
                             ( nd = `5` pred = `6` )
                             ( nd = `6` pred = `3` )
                           ) " XXX the ordering depends on the implementation of the priority queue, watch out
      ).

    endmethod.
endclass.