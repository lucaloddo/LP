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

?- new_heap(H).
Questo predicato asserisce un nuovo heap con grandezza standard fissata
ad 1. Se l'heap che si vuole asserire esiste già nella base di 
conoscenza, allora il predicato non asserisce ma si limita a verificare
l'esistenza dell'heap. 

?- delete_heap(H).
Questo predicato elimina dalla base di conoscenza qualsiasi informazione 
legata all'heap H.

?- heap_has_size(H, S).
Questo predicato è vero quando S è la grandezza dell'heap H.

?- heap_empty(H).
Questo predicato è vero quando l'heap H è vuoto.

?- heap_not_empty(H).
Questo predicato è vero quando l'heap H non è vuoto.

?- heap_head(H, K, V).
Questo predicato è vero quando l'heap H ha come testa la coppia chiave e 
valore rispettivamente uguale a K e V. Prima controlla che l'heap H non 
sia vuoto e poi verifica che l'heap_entry in posizione 1 corrisponda.

?- update_size(H, C).
Questo predicato aggiorna la grandezza dell'heap. Viene incrementata 
la grandezza di un valore pari a C, poi viene eliminato e asserito l'heap
H per tenere aggiornata l'informazione.

?- heap_insert(H, K, V).
Questo predicato è vero quando l'elemento V è inserito nell'heap con 
chiave K. Per questo predicato abbiamo distinto diversi casi:
- con heap vuoto l'elemento viene inserito in testa;
- con heap non vuoto, l'elemento viene inserito in ultima posizione e 
viene lasciato in quel posto perché la chiave dell'elemento è maggiore
della chiave del nodo padre;
- con heap non vuoto, l'elemento viene inserito in ultima posizione e 
viene scambiato perché la chiave dell'elemento è minore della chiave del
nodo padre.
Il terzo caso è il più complesso perché dobbiamo assicurarci che il 
minheap rimanga tale. A questo proposito abbiamo utilizzato il predicato
parental_control che si occupa di tenere ordinato l'heap risalendolo.

?- modify_key(H, NewKey, OldKey, V).
Questo predicato aggiorna l'elemento con valore V e chiave OldKey, 
sostituendo la OldKey con la NewKey. Dopo aver effettuato lo scambio 
utilizziamo due predicati (parental_control e children_control) che, in
caso trovino violate le proprietà del minheap, scambiano gli elementi per
tenerli in ordine corretto.

?- children_control(H, P).
Questo predicato è vero quando gli elementi dell'heap che stanno sotto
alla chiave P non violano le proprietà del minheap. 
Abbiamo previsto 7 diversi casi:
1. P non ha figli, quindi non avvengono cambiamenti;
2. P ha un unico figlio e la chiave P è minore o uguale a quella del 
figlio, quindi non avvengono cambiamenti;
3. P ha un unico figlio e la chiave P è maggiore di quella del figlio,
quindi i due elementi vengono scambiati;
4. P ha due figli (L sta per Left ed R sta per Right), la chiave P è
minore o uguale alla chiave L e la chiave L è minore o uguale alla chiave
R, quindi non avvengono cambiamenti;
5. P ha due figli, la chiave P è maggiore della chiave L e la chiave L è
minore o uguale alla chiave R, quindi l'elemento con chiave P viene
scambiato con l'elemento con chiave L;
6. P ha due figli, la chiave P è minore della chiave R e la chiave R è
minore della chiave L, quindi non avvengono cambiamenti;
7. P ha due figli, la chiave P è maggiore della chiave R e la chiave R è
minore della chiave L, quindi l'elemento con chiave P viene scambiato 
con l'elemento con chiave R.

?- parental_control(H, C).
Questo predicato è vero quando gli elementi dell'heap che stanno sopra la
chiave C non violano le proprietà del minheap.
Abbiamo previsto 3 diversi casi:
1. caso base in cui si interrompe la ricorsione, C è uguale a 1;
2. altro caso base in cui la nostra chiave C è maggiore della chiave del
parent dell'elemento con chiave C, quindi non effettuo scambi e interrompo 
la ricorsione;
3. caso ricorsivo, la chiave C è minore della chiave del parent, quindi 
scambio i due elementi e richiamo il parental_control sul parent.

?- switch(H, PP, C).
Questo predicato scambia l'elemento con chiave PP con l'elemento con
chiave C. In sostanza elimina le vecchie entry e ne asserisce due nuove
nella posizione corretta, al fine di rispettare le proprietà del minheap.

?- heap_extract(H, K, V).
Questo predicato toglie dall'heap l'elemento che sta in testa. Abbiamo 
previsto 3 diversi casi:
1. l'heap è vuoto, quindi il predicato fallisce;
2. l'heap contiene un solo elemento, quindi estraggo l'elemento e non
faccio ulteriori operazioni;
3. l'heap ha più di un elemento, quindi:
	a. estraggo la testa;
	b. inserisco in testa l'ultimo elemento dell'heap;
	c. chiamo il predicato heapify sulla testa dell'heap.

?- heapify(H, S).
Questo predicato controlla che le proprietà del minheap siano rispettate.
H identifica l'heap mentre S è il nodo su cui viene lanciato l'heapify.
Abbiamo previsto 8 diversi casi, precisamente 4 casi basi e 4 ricorsivi:
1. caso base in cui l'heap ha grandezza pari ad 1;
2. caso base in cui l'heapify viene lanciato su un nodo con un solo
figlio. In questo caso controlla se la chiave del figlio è minore di 
quella del padre. Se è minore allora scambia i due nodi.
3. caso base in cui l'heapify viene lanciato su un nodo con un solo
figlio. In questo caso controlla se la chiave del padre è minore o uguale 
a quella del figlio. Se è minore allora la proprietà è rispettata e non
esegue altri predicati.
4. caso base in cui l'heapify viene lanciato su un nodo con due figli.
Entrambi i figli hanno chiavi maggiori o uguali a quella del padre quindi
non avvengono scambi perché la proprietà è rispettata.
5. caso ricorsivo in cui l'heapify viene lanciato su un nodo con due figli.
La chiave del figlio sinistro è minore di quella del padre ma la chiave
del figlio destro è minore di quella del figlio sinistro. Quindi scambio
il nodo padre con il nodo figlio destro.
6. caso ricorsivo in cui l'heapify viene lanciato su un nodo con due figli.
La chiave del figlio sinistro è minore di quella del padre ed è minore o
uguale a quella del figlio destro. Quindi scambio il nodo padre con il 
nodo figlio sinistro.
7. caso ricorsivo in cui l'heapify viene lanciato su un nodo con due figli.
La chiave del padre è minore o uguale a quella del figlio sinsitro ma la 
chiave del figlio destro è minore di quella del padre. Quindi scambio il 
nodo padre con il nodo figlio destro.
8. caso ricorsivo in cui l'heapify viene lanciato su un nodo con due figli.
La chiave del figlio sinistro è minore di quella del padre e la chiave
del padre è minore o uguale a quella del figlio destro. Quindi scambio
il nodo padre con il nodo figlio sinistro.

?- list_heap(H).
Questo predicato lista l'heap H e le heap_entry dell'heap H.

------------------------------------------------------------------------
IMPLEMENTAZIONE INTERFACCIA PER LA MANIPOLAZIONE DI GRAFI

?- new_graph(G).
Questo predicato asserisce un nuovo grafo G. Se il grafo G che si vuole 
asserire, esiste già nella base di conoscenza, allora il predicato non lo
asserisce ma si limita a verificarne l'esistenza.

?- delete_graph(G).
Questo predicato elimina dalla base di conoscenza il grafo G, i vertici
del grafo G e gli archi del grafo G. In caso non esistesse alcun grafo G
il predicato ritorna false.

?- new_vertex(G, V).
Questo predicato asserisce un nuovo vertice V del grafo G. Se il vertice 
esiste già il predicato si limita a verificarne l'esistenza. 

?- graph_vertices(G, Vs).
Questo predicato è vero quando Vs è una lista contenente tutti i vertici 
del grafo G.

?- list_vertices(G).
Questo predicato lista tutti i vertici del grafo G.

?- new_arc(G, U, V).
Questo predicato asserisce un arco con vertici U e V. Il peso viene 
fissato in modo standard ad 1.

?- new_arc(G, U, V, Weight).
Questo predicato asserisce un arco con vertici U e V. In questo caso il 
peso viene specificato dall'argomento Weight. L'unica condizione è che 
Weight sia maggiore di 0.

?- graph_arcs(G, Es).
Questo predicato è vero quando Es è una lista contenente tutti gli archi 
del grafo G.

?- vertex_neighbors(G, V, Ns).
Questo predicato è vero quando la lista Ns è una lista contenente tutti 
gli archi che partono dal nodo V.

?- adjs(G, V, Vs).
Questo predicato è vero quando la lista Vs è una lista contenente tutti 
i vertici direttamente raggiungibili dal nodo V.

?- list_arcs(G).
Questo predicato lista gli archi appartenenti al grafo G.

?- list_graph(G).
Questo predicato lista nome, archi e vertici del grafo G.

?- read_graph(G, Filename).
Questo predicato legge un grafo G da un file csv e lo inserisce nella 
base di dati.

?- assertion(G, Ls).
Questo predicato asserisce i vertici e archi del grafo G presenti nella
lista Ls. Quando Ls è vuota il predicato interrompe la ricorsione.

?- write_graph(G, Filename).
Questo predicato è vero quando G viene scritto su un file csv Filename. 
In questo caso il Type è "graph".

?- write_graph(G, Filename, Type).
Questo predicato è vero quando G viene scritto su un file csv Filename. 
In questo caso il Type è specificato nell'argomento relativo.

?- change_format(As, Bs).
Questo predicato prende un elemento della lista As e lo aggiunge alla
lista Bs rimuovendo il riferimento al grafo. Ad esempio: in As ho
arc(g1, a, b, 4) e in Bs aggiungo arc(a, b, 4).

------------------------------------------------------------------------
IMPLEMENTAZIONE ALGORITMO DI PRIM

?- mst_prim(G, Source).
Questo predicato asserisce nella base dati i predicati 
vertex-key(G, V, K) e vertex-previous(G, V, U) che faranno parte della
soluzione del MST. Con Source identifichiamo il nodo da cui parte la 
ricerca del percorso più breve.

?- extract_all_heap(G, Us).
Questo predicato svuota progessivamente l'heap, rimuovendo dalla testa 

?- prim(G, U, Us, Ns).
Questo predicato 

?- update_prim(G, V, U, W).
Questo predicato 

?- assert_v(G, Source, Ns).
Questo predicato 

?- mst_get(G, Source, PreOrderTree)
Questo predicato ritorna PreOrderTree che è una lista degli archi del MST 
ordinata secondo un attraversamento preorder dello stesso, fatta rispetto 
al peso dell'arco, se si deve ordinare archi con pari peso l'ordinamento
sarà lessicografico.  




























