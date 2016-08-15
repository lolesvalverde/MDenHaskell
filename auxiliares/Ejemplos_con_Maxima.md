# Ejemplos de grafos con Maxima

## Carga de la librería de grafos

~~~
(%i2) load (graphs)$
~~~

## El TAD de los grafos (GrafosConListaDeAristas)

~~~
(%i7) ejGrafo : create_graph ([1,2,3,4,5],[[1,2],[1,4],[1,5],[2,3],[2,5],[3,4],[3,5],[4,5]])$

(%i8) ejGrafo;
(%o8) GRAPH(5 vertices, 8 edges)

(%i9) print_graph (ejGrafo) $
Graph on 5 vertices with 8 edges.
Adjacencies:
  1 :  5  4  2
  2 :  5  3  1
  3 :  5  4  2
  4 :  5  3  1
  5 :  4  3  2  1

(%i10) vertices (ejGrafo);
(%o10) [1, 2, 3, 4, 5]

(%i18) neighbors (4,ejGrafo);
(%o18) [5, 3, 1]

(%i19) neighbors (3,ejGrafo);
(%o19) [5, 4, 2]

(%i20) edges (ejGrafo);
(%o20) [[4, 5], [3, 5], [3, 4], [2, 5], [2, 3], [1, 5], [1, 4], [1, 2]]

(%i21) is_edge_in_graph ([3,5],ejGrafo);
(%o21) true
(%i22) is_edge_in_graph ([5,3],ejGrafo);
(%o22) true
(%i23) is_edge_in_graph ([1,3],ejGrafo);
(%o23) false
~~~

## Ejemplos de grafos

~~~
(%i2) empty_graph (0);
(%o2) GRAPH(0 vertices, 0 edges)

(%i6) c5 : cycle_graph (5)$
(%i7) vertices (c5);
(%o7) [4, 3, 2, 1, 0]
(%i8) edges (c5);
(%o8) [[0, 4], [3, 4], [2, 3], [1, 2], [0, 1]]

(%i9)  k4 : complete_graph (4)$
(%i10) vertices (k4);
(%o10) [3, 2, 1, 0]
(%i11) edges (k4);
(%o11) [[2, 3], [1, 3], [1, 2], [0, 3], [0, 2], [0, 1]]

(%i12) b23 : complete_bipartite_graph (2,3)$
(%i13) vertices (b23);
(%o13) [0, 1, 2, 3, 4]
(%i14) edges (b23);
(%o14) [[1, 4], [1, 3], [1, 2], [0, 4], [0, 3], [0, 2]]

(%i17) r5 : wheel_graph (5)$
(%i18) vertices (r5);
(%o18) [5, 4, 3, 2, 1, 0]
(%i19) edges (r5);
(%o19) [[4,5],[3,5],[2,5],[1,5],[0,5],[0,4],[3,4],[2,3],[1,2],[0,1]]

(%i23) circulante : circulant_graph (6,[1,2])$
(%i24) vertices (circulante);
(%o24) [5, 4, 3, 2, 1, 0]
(%i25) edges (circulante);
(%o25) [[1,5],[0,4],[3,5],[2,4],[1,3],[0,2],[0,5],[4,5],[3,4],[2,3],[1,2],[0,1]]

(%i26) p_4_2 : petersen_graph (4,2)$
(%i27) vertices (p_4_2);
(%o27) [7, 6, 5, 4, 3, 2, 1, 0]
(%i28) edges (p_4_2);
(%o28) [[0,3],[3,7],[2,3],[2,6],[1,2],[5,7],[1,5],[0,1],[4,6],[0,4]]
~~~

## Definiciones y propiedades de grafos

### Definiciones de grafos

~~~
(%i29) graph_order (cycle_graph (4));
(%o29) 4

(%i30) graph_size (cycle_graph (4));
(%o30) 4
(%i31) graph_size (petersen_graph (5,2));
(%o31) 15

(%i37) neighbors (2,create_graph ([1,2,3,4,5],[[1,2],[1,3],[2,3]]));
(%o37) [3, 1]

(%i38) vertex_degree (2,create_graph ([1,2,3,4,5],[[1,2],[1,3],[2,3]]));
(%o38) 2

(%i39) min_degree (create_graph ([1,2,3,4],[[1,2],[1,3],[2,4]]));
(%o39) [1, 3]
(%i40) first (min_degree (create_graph ([1,2,3,4],[[1,2],[1,3],[2,4]])));
(%o40) 1

(%i41) max_degree (create_graph ([1,2,3,4],[[1,2],[1,3],[2,4]]));
(%o41) [2, 1]
(%i42) first (max_degree (create_graph ([1,2,3,4],[[1,2],[1,3],[2,4]])));
(%o42) 2

(%i48) is_graph (create_graph ([1,2,3],[[1,2],[1,3],[2,3]]));
(%o48) true
(%i49) is_graph (create_graph ([1,2,3],[[1,2],[1,1],[2,3]]));
Argument 1 to add_edge is not an edge (2).
 -- an error. To debug this try: debugmode(true);

(%i50) degree_sequence (create_graph ([1,2,3,4,5],[[1,2],[1,3],[1,4],[2,4]])); 
(%o50) [0, 1, 2, 2, 3]
~~~

### Operaciones y propiedades sobre grafos

~~~
(%i5)  g : create_graph ([1,2,3,4],[[1,2],[1,4],[2,4]])$
(%i6)  remove_edge ([1,4],g)$
(%i8)  vertices (g);
(%o8)  [1, 2, 3, 4]
(%i9)  edges (g);
(%o9)  [[2, 4], [1, 2]]
(%i10) g : create_graph ([1,2,3,4],[[1,2],[1,3],[2,4]])$
(%i11) remove_edge ([1,3],g)$
(%i12) vertices (g);
(%o12) [1, 2, 3, 4]
(%i13) edges (g);
(%o13) [[2, 4], [1, 2]]

(%i14) g : create_graph ([1,2,3,4],[[1,2],[1,4],[2,4]])$
(%i15) remove_vertex (1,g)$
(%i16) vertices (g);
(%o16) [2, 3, 4]
(%i17) edges (g);
(%o17) [[2, 4]]

(%i20) g : cycle_graph (5);
(%o20) GRAPH(5 vertices, 5 edges)
(%i21) g : cycle_graph (5)$
(%i22) edges (g);
(%o22) [[0, 4], [3, 4], [2, 3], [1, 2], [0, 1]]
(%i23) add_edge([0,2],g)$
(%i24) edges (g);
(%o24) [[0, 2], [0, 4], [3, 4], [2, 3], [1, 2], [0, 1]]

(%i56) g1 : create_graph ([1,2,3,4], [[1,3],[2,3]])$
(%i57) g2: complement_graph (g1)$
(%i58) [vertices (g2), edges (g2)];
(%o58) [[4, 3, 2, 1], [[3, 4], [2, 4], [1, 4], [1, 2]]]
~~~

## Morfismos

~~~
(%i59) g1 : create_graph ([1,2,3], [[1,3],[2,3]])$
(%i60) g2 : create_graph ([2,4,6], [[2,6],[4,6]])$
(%i61) isomorphism (g1,g2);
(%i64) g3 : create_graph ([1,2,3,4], [[1,2],[2,3],[3,4]])$
(%i65) g4 : create_graph ([1,2,3,4], [[1,2],[1,3],[1,4]])$
(%i66) isomorphism (g3,g4);
(%o66) []

(%i67) g1 : create_graph ([1,2,3], [[1,3],[2,3]])$
(%i68) g2 : create_graph ([2,4,6], [[2,6],[4,6]])$
(%i69) is_isomorphic (g1,g2);
(%o69) true
(%i70) g3 : create_graph ([1,2,3,4], [[1,2],[2,3],[3,4]])$
(%i71) g4 : create_graph ([1,2,3,4], [[1,2],[1,3],[1,4]])$
(%i72) is_isomorphic (g3,g4);
(%o72) false
~~~

## Conectividad de grafos

~~~
(%i74) shortest_path (0,3,cycle_graph (4));
(%o74) [0, 3]
(%i75) shortest_path (3,0,cycle_graph (4));
(%o75) [3, 0]
(%i76) shortest_path (1,4,create_graph ([1,2,3,4],[[1,2],[3,4]]));
(%o76) []

(%i77) g : create_graph ([1,2,3,4],[[1,2],[2,3]])$
(%i78) vertex_distance (1,1,g);
(%o78) 0
(%i79) vertex_distance (1,2,g);
(%o79) 1
(%i80) vertex_distance (1,3,g);
(%o80) 2
(%i81) vertex_distance (1,4,g);
(%o81) inf

(%i84) g1 : create_graph ([1,2,3,4,5],[[1,2],[2,3]])$
(%i85) connected_components (g1);
(%o85) [[5], [4], [3, 2, 1]]
(%i86) g2 : create_graph ([1,2,3,4,5],[[1,2],[2,3],[4,5]])$
(%i87) connected_components (g2);
(%o87) [[5, 4], [3, 2, 1]]

(%i93) g1 : create_graph ([1,2,3],[[1,2],[2,3]])$
(%i94) is_connected (g1);
(%o94) true
(%i95) g2 : create_graph ([1,2,3,4,5],[[1,2],[2,3],[4,5]])$
(%i96) is_connected (g2);
(%o96) false

(%i97) g1 = create_graph ([1,2,3], [[1,2],[2,3]])$
(%i98) vertex_eccentricity (1,g1);
(%o98) 2
(%i99) vertex_eccentricity (2,g1);
(%o99) 1
(%i100) g2 = create_graph ([1,2,3,4], [[1,2],[2,3]])$
(%i101) vertex_eccentricity (2,g2);
eccentricity: graph is not connected.
 -- an error. To debug this try: debugmode(true);

(%i2) diameter (create_graph ([1,2,3],[[1,2],[2,3]]));
(%o2) 2
(%i3) diameter (create_graph ([1,2,3],[[1,2]]));
eccentricity: graph is not connected.
 -- an error. To debug this try: debugmode(true);

(%i4) radius (create_graph ([1,2,3],[[1,2],[2,3]]));
(%o4) 1
(%i5) radius (create_graph ([1,2,3],[[1,2]]));
eccentricity: graph is not connected.
 -- an error. To debug this try: debugmode(true);

(%i8) graph_center (create_graph ([1,2,3],[[1,2],[2,3]]));
(%o8) [2]
(%i9) graph_center (create_graph ([1,2,3],[[1,2],[1,3],[2,3]]));
(%o9) [1, 2, 3]

(%i10) girth (cycle_graph (5));
(%o10) 5
(%i11) girth (complete_graph (5));
(%o11) 3
(%i12) girth (create_graph ([1,2,3],[[1,2],[2,3]]));
(%o12) inf
~~~
