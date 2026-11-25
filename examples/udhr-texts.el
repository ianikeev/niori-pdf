;;; udhr-texts.el --- UDHR sample texts for niori-pdf examples -*- lexical-binding: t; -*-

;; First two paragraphs of the preamble of the Universal Declaration
;; of Human Rights, used as hyphenation example texts in the sample
;; report and the evaluation report.
;;
;; Text (c) United Nations.  Transcriptions taken from the UDHR in
;; XML project (https://github.com/eric-muller/udhr, database
;; originally at https://www.unicode.org/udhr).

;; Copyright (C) 2026 niori-pdf authors

;;; Code:

(defconst udhr-texts--american
  ;; English (US patterns)
  '(
    "Whereas recognition of the inherent dignity and of the equal and inalienable rights of all members of the human family is the foundation of freedom, justice and peace in the world,"
    "Whereas disregard and contempt for human rights have resulted in barbarous acts which have outraged the conscience of mankind, and the advent of a world in which human beings shall enjoy freedom of speech and belief and freedom from fear and want has been proclaimed as the highest aspiration of the common people,"
    )
  "UDHR preamble, first two paragraphs: English (US patterns).")

(defconst udhr-texts--british
  ;; English (UK patterns)
  '(
    "Whereas recognition of the inherent dignity and of the equal and inalienable rights of all members of the human family is the foundation of freedom, justice and peace in the world,"
    "Whereas disregard and contempt for human rights have resulted in barbarous acts which have outraged the conscience of mankind, and the advent of a world in which human beings shall enjoy freedom of speech and belief and freedom from fear and want has been proclaimed as the highest aspiration of the common people,"
    )
  "UDHR preamble, first two paragraphs: English (UK patterns).")

(defconst udhr-texts--german
  ;; German
  '(
    "Da die Anerkennung der angeborenen Würde und der gleichen und unveräußerlichen Rechte aller Mitglieder der Gemeinschaft der Menschen die Grundlage von Freiheit, Gerechtigkeit und Frieden in der Welt bildet,"
    "da die Nichtanerkennung und Verachtung der Menschenrechte zu Akten der Barbarei geführt haben, die das Gewissen der Menschheit mit Empörung erfüllen, und da verkündet worden ist, dass einer Welt, in der die Menschen Rede- und Glaubensfreiheit und Freiheit von Furcht und Not genießen, das höchste Streben des Menschen gilt,"
    )
  "UDHR preamble, first two paragraphs: German.")

(defconst udhr-texts--french
  ;; French
  '(
    "Considérant que la reconnaissance de la dignité inhérente à tous les membres de la famille humaine et de leurs droits égaux et inaliénables constitue le fondement de la liberté, de la justice et de la paix dans le monde,"
    "Considérant que la méconnaissance et le mépris des droits de l’homme ont conduit à des actes de barbarie qui révoltent la conscience de l’humanité et que l’avènement d’un monde où les êtres humains seront libres de parler et de croire, libérés de la terreur et de la misère, a été proclamé comme la plus haute aspiration de l’homme,"
    )
  "UDHR preamble, first two paragraphs: French.")

(defconst udhr-texts--spanish
  ;; Spanish
  '(
    "Considerando que la libertad, la justicia y la paz en el mundo tienen por base el reconocimiento de la dignidad intrínseca y de los derechos iguales e inalienables de todos los miembros de la familia humana;"
    "Considerando que el desconocimiento y el menosprecio de los derechos humanos han originado actos de barbarie ultrajantes para la conciencia de la humanidad, y que se ha proclamado, como la aspiración más elevada del hombre, el advenimiento de un mundo en que los seres humanos, liberados del temor y de la miseria, disfruten de la libertad de palabra y de la libertad de creencias;"
    )
  "UDHR preamble, first two paragraphs: Spanish.")

(defconst udhr-texts--italian
  ;; Italian
  '(
    "Considerato che il riconoscimento della dignità inerente a tutti i membri della famiglia umana e dei loro diritti, uguali ed inalienabili, costituisce il fondamento della libertà, della giustizia e della pace nel mondo;"
    "Considerato che il disconoscimento e il disprezzo dei diritti umani hanno portato ad atti di barbarie che offendono la coscienza dell'umanità, e che l'avvento di un mondo in cui gli esseri umani godano della libertà di parola e di credo e della libertà dal timore e dal bisogno è stato proclamato come la più alta aspirazione dell'uomo;"
    )
  "UDHR preamble, first two paragraphs: Italian.")

(defconst udhr-texts--portuguese
  ;; Portuguese (Brazilian text)
  '(
    "Considerando que o reconhecimento da dignidade inerente a todos os membros da família humana e de seus direitos iguais e inalienáveis é o fundamento da liberdade, da justiça e da paz no mundo,"
    "Considerando que o desprezo e o desrespeito pelos direitos humanos resultaram em atos bárbaros que ultrajaram a consciência da Humanidade e que o advento de um mundo em que os todos gozem de liberdade de palavra, de crença e da liberdade de viverem a salvo do temor e da necessidade foi proclamado como a mais alta aspiração do ser humano comum,"
    )
  "UDHR preamble, first two paragraphs: Portuguese (Brazilian text).")

(defconst udhr-texts--portuguese-pt
  ;; Portuguese (European text)
  '(
    "Considerando que o reconhecimento da dignidade inerente a todos os membros da família humana e dos seus direitos iguais e inalienáveis constitui o fundamento da liberdade, da justiça e da paz no mundo;"
    "Considerando que o desconhecimento e o desprezo dos direitos humanos conduziram a actos de barbárie que revoltam a consciência da Humanidade e que o advento de um mundo em que os seres humanos sejam livres de falar e de crer, libertos do terror e da miséria, foi proclamado como a mais alta inspiração do ser humano;"
    )
  "UDHR preamble, first two paragraphs: Portuguese (European text).")

(defconst udhr-texts--esperanto
  ;; Esperanto
  '(
    "Pro tio, ke agnosko de la esenca digno kaj de la egalaj kaj nefordoneblaj rajtoj de ĉiuj membroj de la homara familio estas la fundamento de libero, justo kaj paco en la mondo,"
    "Pro tio, ke malagnosko kaj malestimo de la homaj rajtoj rezultigis barbarajn agojn, kiuj forte ofendis la konsciencon de la homaro, kaj ke la efektiviĝo de tia mondo, en kiu la homoj ĝuos liberecon de parolo kaj de kredo kaj liberiĝon el timo kaj bezono, estas proklamita kiel la plej alta aspiro de ordinaraj homoj,"
    )
  "UDHR preamble, first two paragraphs: Esperanto.")

(defconst udhr-texts--ukrainian
  ;; Ukrainian
  '(
    "Беручи до уваги, що визнання гідності, яка властива всім членам людської сім'ї, і рівних та невід'ємних їх прав є основою свободі, справедливості та загального миру; і"
    "беручи до уваги, що зневажання і нехтування правами людини призвели до варварських актів, які обурюють совість людства, і що створення такого світу, в якому люди будуть мати свободу слова і переконань і будуть вільні від страху і нужди, проголошено як високе прагнення людей; і"
    )
  "UDHR preamble, first two paragraphs: Ukrainian.")

(defconst udhr-texts--russian
  ;; Russian
  '(
    "Принимая во внимание, что признание достоинства, присущего всем членам человеческой семьи, и равных и неотъемлемых прав их является основой свободы, справедливости и всеобщего мира; и"
    "принимая во внимание, что пренебрежение и презрение к правам человека привели к варварским актам, которые возмущают совесть человечества, и что создание такого мира, в котором люди будут иметь свободу слова и убеждений и будут свободны от страха и нужды, провозглашено как высокое стремление людей; и"
    )
  "UDHR preamble, first two paragraphs: Russian.")

(provide 'udhr-texts)
;;; udhr-texts.el ends here
