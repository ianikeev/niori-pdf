;;; hyphenation-texts.el --- Literary sample texts for niori-pdf -*- lexical-binding: t; -*-

;; Original-language literary excerpts for the hyphenation demonstration
;; in sample-report.el. Each variable contains two paragraphs.
;;
;; The first-publication date identifies the work, not necessarily the
;; edition used for this transcription. The source editions are recorded
;; below each entry.
;;
;; These are short excerpts from historical works whose authors are
;; deceased for more than 70 years. The status of a particular digital
;; transcription or edition may have separate terms; see the project
;; THIRD-PARTY-NOTICES before redistributing this file.
;;
;; Sources:
;;
;; American English:
;;   Henry David Thoreau, "Walking" (first published in The Atlantic
;;   Monthly, June 1862). Text checked against the 1863 Excursions
;;   transcription:
;;   https://en.wikisource.org/wiki/Excursions_\(1863\)/Walking
;;
;; British English:
;;   Jerome K. Jerome, Three Men in a Boat (1889), J. W. Arrowsmith,
;;   Bristol. Project Gutenberg transcription:
;;   https://www.gutenberg.org/ebooks/308
;;
;; German:
;;   Theodor Storm, Der Schimmelreiter (1888). TextGrid Repository
;;   transcription. Historical spelling is retained as presented there:
;;   https://www.textgridrep.org/browse/vtb3.0\?lang\=de
;;
;; French:
;;   Guy de Maupassant, Le Horla (1887). Project Gutenberg transcription:
;;   https://www.gutenberg.org/ebooks/10775
;;
;; Spanish:
;;   Benito Pérez Galdós, Fortunata y Jacinta (1887), Madrid,
;;   Imprenta de La Guirnalda. Digital edition based on the 1887 edition:
;;   https://www.cervantesvirtual.com/obra/fortunata-y-jacinta-dos-historias-de-casadas--0/
;;
;; Italian:
;;   Carlo Collodi, Le avventure di Pinocchio (first published in book
;;   form in 1883). The excerpt here follows the 1902 Bemporad edition,
;;   not the first edition:
;;   https://www.gutenberg.org/ebooks/52484
;;
;; Portuguese:
;;   Machado de Assis, O Alienista (published in A Estação in 1882).
;;   Machado de Assis Digital Corpus Project, Brigham Young University:
;;   https://machado.byu.edu/text/o-alienista/
;;
;; Esperanto:
;;   L. L. Zamenhof, Dua Libro de l' Lingvo Internacia (1888).
;;   Project Gutenberg transcription:
;;   https://www.gutenberg.org/ebooks/20006
;;
;; Ukrainian:
;;   Mykhailo Kotsiubynsky, Intermezzo (written in 1908; first published
;;   in 1909). Working transcription follows the 1955 collected-works
;;   edition, volume II:
;;   https://uk.wikisource.org/wiki/Твори_(Коцюбинський,_1955)/2/Intermezzo
;;
;; Russian:
;;   Anton Chekhov, Дама с собачкой (first published in 1899).
;;   Transcription follows the 1986 collected works, volume 10:
;;   https://ilibrary.ru/text/976/index.html

(defconst hyphenation-texts--american
  '("I wish to speak a word for Nature, for absolute freedom and wildness, as contrasted with a freedom and culture merely civil,—to regard man as an inhabitant, or a part and parcel of Nature, rather than a member of society. I wish to make an extreme statement, if so I may make an emphatic one, for there are enough champions of civilization: the minister and the school-committee, and every one of you will take care of that."
    "I have met with but one or two persons in the course of my life who understood the art of Walking, that is, of taking walks,—who had a genius, so to speak, for sauntering.")
  "Two excerpts from Henry David Thoreau's Walking.")

(defconst hyphenation-texts--british
  '("There were four of us—George, and William Samuel Harris, and myself, and Montmorency. We were sitting in my room, smoking, and talking about how bad we were—bad from a medical point of view I mean, of course."
    "We were all feeling seedy, and we were getting quite nervous about it. Harris said he felt such extraordinary fits of giddiness come over him at times, that he hardly knew what he was doing; and then George said that he had fits of giddiness too, and hardly knew what he was doing.")
  "Two excerpts from Jerome K. Jerome's Three Men in a Boat.")

(defconst hyphenation-texts--german
  '("Was ich zu berichten beabsichtige, ist mir vor reichlich einem halben Jahrhundert im Hause meiner Urgroßmutter, der alten Frau Senator Feddersen, kundgeworden, während ich, an ihrem Lehnstuhl sitzend, mich mit dem Lesen eines in blaue Pappe eingebundenen Zeitschriftenheftes beschäftigte; ich vermag mich nicht mehr zu entsinnen, ob von den »Leipziger« oder von »Pappes Hamburger Lesefrüchten«."
    "Noch fühl ich es gleich einem Schauer, wie dabei die linde Hand der über Achtzigjährigen mitunter liebkosend über das Haupthaar ihres Urenkels hinglitt.")
  "Two excerpts from Theodor Storm's Der Schimmelreiter.
The TextGrid transcription's historical spelling is retained.")

(defconst hyphenation-texts--french
  '("Quelle journée admirable ! J'ai passé toute la matinée étendu sur l'herbe, devant ma maison, sous l'énorme platane qui la couvre, l'abrite et l'ombrage tout entière. J'aime ce pays, et j'aime y vivre parce que j'y ai mes racines, ces profondes et délicates racines, qui attachent un homme à la terre où sont nés et morts ses aïeux, qui l'attachent à ce qu'on pense et à ce qu'on mange, aux usages comme aux nourritures, aux locutions locales, aux intonations des paysans, aux odeurs du sol, des villages et de l'air lui-même."
    "J'aime ma maison où j'ai grandi. De mes fenêtres, je vois la Seine qui coule, le long de mon jardin, derrière la route, presque chez moi, la grande et large Seine, qui va de Rouen au Havre, couverte de bateaux qui passent.")
  "Two excerpts from Guy de Maupassant's Le Horla.")

(defconst hyphenation-texts--spanish
  '("Las noticias más remotas que tengo de la persona que lleva este nombre me las ha dado Jacinto María Villalonga, y alcanzan al tiempo en que este amigo mío y el otro y el de más allá, Zalamero, Joaquinito Pez, Alejandro Miquis, iban a las aulas de la Universidad. No cursaban todos el mismo año, y aunque se reunían en la cátedra de Camús, separábanse en la de Derecho Romano: el chico de Santa Cruz era discípulo de Novar, y Villalonga de Coronado."
    "Ni tenían todos el mismo grado de aplicación: Zalamero, juicioso y circunspecto como pocos, era de los que se ponen en la primera fila de bancos, mirando con faz complacida al profesor mientras explica, y haciendo con la cabeza discretas señales de asentimiento a todo lo que dice.")
  "Two excerpts from Benito Pérez Galdós's Fortunata y Jacinta.")

(defconst hyphenation-texts--italian
  '("— C'era una volta.... — Un re! — diranno subito i miei piccoli lettori. — No, ragazzi, avete sbagliato. C'era una volta un pezzo di legno."
    "Non era un legno di lusso, ma un semplice pezzo da catasta, di quelli che d'inverno si mettono nelle stufe e nei caminetti per accendere il fuoco e per riscaldare le stanze.")
  "Two excerpts from Carlo Collodi's Le avventure di Pinocchio.
The transcription follows the 1902 Bemporad edition.")

(defconst hyphenation-texts--portuguese
  '("As crônicas da vila de Itaguaí dizem que em tempos remotos vivera ali um certo médico, o Dr. Simão Bacamarte, filho da nobreza da terra e o maior dos médicos do Brasil, de Portugal e das Espanhas. Estudara em Coimbra e Pádua. Aos trinta e quatro anos regressou ao Brasil, não podendo el rei alcançar dele que ficasse em Lisboa, expedindo os negócios da monarquia."
    "— A ciência — disse ele a Sua Majestade — é o meu emprego único; Itaguaí é o meu universo.")
  "Two excerpts from Machado de Assis's O Alienista.")

(defconst hyphenation-texts--esperanto
  '("Elirante ankoraŭ unu fojon antaŭ la estimata publiko, mi sentas la devon antaŭ ĉio danki la legantan publikon por la viva kunsento, kiun ĝi montris por mia afero. La multaj promesoj, kiujn mi ricevas, kaj el kiuj tre granda parto estas subskribita \"senkondiĉe\", la leteroj kun kuraĝigoj aŭ konsiloj—ĉio tio ĉi montras al mi, ke mia profunda kredo je l' homaro min ne trompis."
    "La bona genio de l' homaro vekiĝis: de ĉiuj flankoj al la laboro ĉiuhoma venas amasoj, kiuj ordinare estas tiel maldiligentaj por ĉia nova afero; junaj kaj maljunaj, viroj kaj virinoj—rapidas porti ilian ŝtonon por la granda, grava kaj utilega konstruo. Vivu l' homaro, vivu la frateco de l' popoloj, vivu eterne!")
  "Two excerpts from L. L. Zamenhof's Dua Libro de l' Lingvo Internacia.")

(defconst hyphenation-texts--ukrainian
  '("Лишилось тільки ще спакуватись… Се було одно з тих незчисленних «треба», які мене так утомили і не давали спати. Дарма, чи те «треба» мале, чи велике — вагу те має, що кожен раз воно вимагає уваги, що не я їм, а воно мною уже керує. Фактично — стаєш невільником сього многоголового звіра. Хоч на час увільнитись від нього, забути, спочити. Я утомився."
    "Бо життя безупинно і невблаганно іде на мене, як хвиля на берег. Не тільки власне, а і чуже. А врешті — хіба я знаю, де кінчається власне життя, а чуже починається? Я чую, як чуже існування входить в моє, мов повітря крізь вікна і двері, як води притоків у річку.")
  "Two excerpts from Mykhailo Kotsiubynsky's Intermezzo.
The working text follows the 1955 collected-works edition, volume II.")

(defconst hyphenation-texts--russian
  '("Говорили, что на набережной появилось новое лицо: дама с собачкой. Дмитрий Дмитрич Гуров, проживший в Ялте уже две недели и привыкший тут, тоже стал интересоваться новыми лицами. Сидя в павильоне у Берне, он видел, как по набережной прошла молодая дама, невысокого роста блондинка, в берете; за нею бежал белый шпиц."
    "И потом он встречал ее в городском саду и на сквере по нескольку раз в день. Она гуляла одна, всё в том же берете, с белым шпицем; никто не знал, кто она, и называли ее просто так: дама с собачкой.")
  "Two excerpts from Anton Chekhov's Дама с собачкой.")

(provide 'hyphenation-texts)
;;; hyphenation-texts.el ends here
