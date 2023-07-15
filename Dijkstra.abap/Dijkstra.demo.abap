*&---------------------------------------------------------------------*
*& Report ZDIJK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*& Demo for usage of the dijkstra include, also see unit tests
*&---------------------------------------------------------------------*
REPORT ZDIJK.

INCLUDE ZINCLDIJK.

"-----------------------------------------------------------------------------------------------------------
" Main
"-----------------------------------------------------------------------------------------------------------
start-of-selection.

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
      data(r_graph) = new lcl_adjacency_list( it_graph ).
      data(r_dijkstra) = new lcl_dijkstra( r_graph = r_graph ).
      data it_paths type lcl_dijkstra=>tt_pred.
      call method r_dijkstra->run exporting start_node_name = `1` importing it_path = it_paths.

      loop at it_paths reference into data(r_part).
        write: 'from', r_part->*-pred, 'to', r_part->*-nd.
        new-line.
      endloop.