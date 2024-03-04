README.txt

------------------------------------------------------------------------
Progetto appello di Gennaio 2020 svolto da:
844526 Kolyszko Matteo
844529 Loddo Luca
845374 Arizzi Sara

------------------------------------------------------------------------
INTRODUZIONE 

Nome progetto: mst.pl

Obiettivo: implementare l'algoritmo di Prim per risolvere il problema 
del Minimum Spanning Tree. 
Il problema del MST consiste nel calcolare il percorso più breve che 
collega tutti i punti di un albero. Il grafo su cui viene testato 
l'algoritmo è un grafo non diretto, connesso ed etichettato con pesi 
non negativi.

Per svolgere il progetto abbiamo sfruttato nozioni teoriche presenti nei
corsi di:
- Algoritmi e Strutture Dati;
- Analisi e Progetto di Algoritmi.

------------------------------------------------------------------------
IMPLEMENTAZIONE MINHEAP

- (new-heap heap-id &optional (capacity 42)) —> heap-rep 
Questa funzione inserisce un nuovo heap nella hash-table *heaps*.

- (heap-id (heap-rep))
Questa funzione ritorna il nome dell’heap rappresentato dall’heap-rep passata.

- (heap-size (heap-rep))
Questa funzione restituisce la grandezza dell’heap rappresentato dall’heap-rep 
passata.

- (heap-actual-heap (heap-rep))
Questa funzione restituisce la lista degli elementi dell’heap rappresentato 
dall’heap-rep passata.

- (is-heap (heap-id)) —> T
Questa funzione restituisce T se l’heap passato esiste. 

- (heap-delete (heap-id)) —> T
Rimuove tutto l’heap rappresentato dall’heap-id. 

- (heap-empty (heap-id)) —> boolean
Questo predicato è vero quando lo heap heap-id non contiene elementi. 

- (heap-not-empty (heap-id)) —> boolean
Questo predicato è vero quando lo heap heap-id contiene almeno un elemento.

- (heap-head (heap-id)) —> (K V)
Questa funzione ritorna una lista di 2 elementi dove K è la chiave minima e V 
il valore associato. 

- (heap-insert (heap-id K V)) —> boolean
Questa funzione inserisce l’elemento V nello heap heap-id con chiave K. Ad ogni 
inserimento  lo heap viene ristrutturato in modo da mantenere le proprietà 
del minheap.

- (parental-control (heap-id I K V))
Questa funzione controlla che le proprietà del minheap sia rispettata. 

- (parent (I))
Questa funzione restituisce la posizione del nodo parent di I.

- (change-size (heap-id A))
Questa funzione aggiorna la grandezza dell’heap heap-id incrementandola di un 
valore pari ad A.

- (get-value (heap-id I))
Questa funzione ritorna il valore dell’heap heap-id posto in posizione I.

 - (set-value (heap-id I V))
Questa funzione definisce il valore V  in posizione I nell’heap heap-id.

- (heap-extract (heap-id)) —> (K V)
Questa funzione ritorna la lista con K, V e con K minima; la coppia è 
rimossa dallo heap heap-id. Ad ogni rimozione lo heap viene ristrutturato in 
modo da mantenere le proprietà del minheap.

- (heapify (heap-id I K V))
Questa funzione esegue gli scambi tra i nodi in modo da mantenere la 
proprietà del minheap.

- (heap-modify-key (heap-id new-key old-key V))
Questa funzione sostituisce la chiave old-key  con new-key . Dopo ogni 
sostituzione lo heap viene ristrutturato in modo da mantenere le proprietà 
del minheap.

- (get-index-key (heap-id I V))
Questa funzione restituisce la posizione della chiave K con valore V 
nell’heap heap-id.

- (get-key (heap-id I V))
Questa funzione restituisce la chiave in posizione I con valore V 
nell’heap heap-id.

- (heap-print (heap-id)) —> boolean 
Questa funzione stampa sulla console lo stato interno dello heap heap-id.

-(array-print (heap-id I)) 
Questa funzione stampa gli elementi dell’heap heap-id.

------------------------------------------------------------------------------
IMPLEMENTAZIONE DELL'INTERFACCIA PER LA MANIPOLAZIONE DI GRAFI

- (is-graph (graph-id))
Questa funzione ritorna il graph-id stesso se il grafo è già stato creato. 
NIL altrimenti.

- (new-graph (graph-id))
Questa funzione genera un nuovo grafo e lo inserisce nell’hash-table dei grafi 
se il grafo non esiste già.

- (delete-graph (graph-id)) —> NIL
Questa funzione rimuove l’intero grafo dal sistema quindi rimuove tutte le 
istanze presenti nelle hash-tables.

- (is-vertex (graph-id vertex-id)) 
Questa funzione controlla l’esistenza del vertice vertex-id.

- (new-vertex (graph-id vertex-id)) —> vertex-rep
Questa funzione aggiunge un nuove vertice vertex-id al grafo graph-id. Quindi 
aggiunge una istanza nell’hah-table *vertices*.

- (graph-vertices (graph-id vertex-id)) —> vertex-rep-list
Questa funzione ritorna una lista di vertici del grafo graph-id.

- (is-arc (graph-id vertex-1-id vertex-2-id))
Questa funzione verifica l’esistenza dell’arco che va da vertex-1-id a 
vertex-2-id o viceversa.

- (new-arc (graph-id vertex-1-id vertex-2-id &optional (weight 1))) —> arc-rep
Questa funzione aggiunge un arco del grafo graph-id nella hash-table *arcs*. 
L’aggiunta di un arco già esistente ma con peso diverso comporta l’aggiornamento 
dello stesso.

- (graph-arcs (graph-id)) —> arc-rep-list 
Questa funzione ritorna una lista di tutti gli archi presenti nel grafo graph-id.

- (graph-vertex-neighbors (graph-id vertex-id)) —>  arc-rep-list
Questa funzione ritorna una lista di archi che portano dal vertex-id ai vertici 
da esso raggiungibili.

- (graph-vertex-adjacent (graph-id vertex-id)) —>  vertex-rep-list
Questa funzione ritorna una lista di vertici direttamente raggiungibili dal 
vertice vertex-id.

- (graph-print (graph-id))
Questa funzione stampa alla console una lista dei vertici e degli archi del 
grafo graph-id.

--------------------------------------------------------------------------------
IMPLEMENTAZIONE ALGORITMO DI PRIM

- (member-2 (x xs))
Questa funzione ritorna T se l’elemento x è contenuto nella lista xs.

- (mst-prim (graph-id source)) —> NIL
Dopo l’esecuzione di questa funzione l’hash-table *vertex-key* contiene al suo 
interno le associazioni tra (graph-id V) e peso minimo per ogni V appartenente 
al grafo graph-id. La hash-table *previous* contiene le associazioni tra 
(graph-id V) e il previous di V calcolate durante l’esecuzione dell’algoritmo 
di Prim.

- (reset (graph-id))
Questa funzione pulisce le hash-tables *previous* e *vertex-keys*.

- (extract-all-heap (graph-id vertex-rep-list))
Questa funzione estrae uno a uno gli elementi dell’heap fino a svuotarlo. In 
particolare:
1) ogni elemento estratto viene inserito in una lista contenente i vertici 
visitati; 
2) l’elemento estratto viene passato alla funzione prim insieme alla lista 
sopra citata.

-(prim (graph-id *lista-heap* start-vertex neighbors))
Questa funzione aggiorna i pesi dei vertex-key nel caso in cui quelli presenti 
nell’hash-table abbiano peso maggiore rispetto a quelli correnti. Oltre ad aver 
aggiornato il peso aggiorna anche i vertici nel vertex-previous corrispondente.

- (update-prim (graph-id v u w))
Questa funzione aggiorna le informazioni nella hash-table, viene utilizzata 
dalla funzione prim quando i pesi nelle hash-table sono maggiori rispetto a 
quelli correnti.

- (initialize-prim (graph-id source vertex-rep-list))
Questa funzione inizializza le hash-table. Il peso iniziale delle vertex-key 
è MOST-POSITIVE-DOUBLE-FLOAT eccetto per la source che è 0. Per ogni previous 
il padre viene settato a NIL.
 
- (mst-vertex-key (graph-id vertex-id)) —> K
Questa funzione ritorna, durante e dopo l’esecuzione dell’algoritmo di prim, 
il peso minimo di arco che connette vertex-id nell’albero minimo, se queso 
arco non esiste allora K è MOST-POSITIVE-DOUBLE-FLOAT.

- (mst-previous (graph-id vertex-id)) —> U
Questa funzione ritorna il vertice U che è il vertice “genitore” di v nel mst. 

- (mst-get (graph-id source)) —> preorder-mst
Questa funzione ritorna preorder-mst che è una lista degli archi del mst 
ordinata un attraversamento preorder dello stesso fatta rispetto al peso 
dell’arco. Gli archi con pari peso sono ordinati secondo l’ordinamento 
lessicografico del vertice target.

- (find-child (graph-id parent))
Questa funzione ritorna la lista ordinata dei figli di parent di vertex-id.

- (empty-list (graph-id lista-child))
Questa funzione scorre il grafo graph-id in modo preorder e aggiunge 
progressivamente ogni elemento alla lista preorder-mst.

- (has-child (graph-id vertex-id))
Questa funzione ritorna T se il vertex-id ha figli. NIL altrimenti.

- (add (graph-id v1 v2))
Questa funzione aggiunge l’arco tra v1 e v2 alla lista preorder-mst.

- (remove-first (lista-child))
Questa funzione toglie il primo elemento dalla lista lista-child.

- (and-then (peso lettera))
Questa funzione è utilizzata per creare l’ordinamento degli archi prima 
secondo il peso e poi, con peso uguale, secondo l’ordinamento lessicografico. 
Questa funzione gode di altre 4 funzioni di appoggio che sono term-coefficient, 
coefficient<, term-variable, variable< .
