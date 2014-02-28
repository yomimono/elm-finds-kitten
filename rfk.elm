import Window
import Keyboard
import List
import Generator
import Generator.Standard

--todos:
--actual collision detection (done)
--randomly placed objects
--vi key navigation
--endgame detection (done)
--endgame animation (done)
--edge detection (done?)

type Item a = { a | char:String, description:String, xd:Int, yd:Int, cd:Color, isKitten:Bool }
type Colliding b = { b | collidingWith: String }
robot = { char = "@", xd = 0, yd = 0, 
  description = "Robot, sans kitten.", collidingWith = "", 
  isKitten = False, cd = white }
characters = "$%^&*()qwertyuiop[]{}asdfghjkl;:zxcvbnm,.<>"
kittenDescription = "You found kitten!  Good job, robot!"
  
fontify : Color -> String -> Text
fontify col x = Text.color col ( monospace ( toText x) )

samePlace : Item a -> Item b -> Bool
samePlace robot item = robot.xd == item.xd && robot.yd == item.yd

--Determine the case where the robot is investigating an object.
--The robot shouldn't be able to collide with multiple things simultaneously,
--but if this does occur, we'll just take the first object.
collision : Item b -> [Item a] -> Maybe (Item a)
collision robot items = 
  let found = (List.filter (samePlace robot) items )
  in if List.isEmpty found then Nothing else Just (head found)

--convenience function for looking up what message ought to be displayed
--based on whether our robot's just investigated something and hasn't yet 
--moved away.
getMessage : Colliding a -> Element
getMessage r = Text.text (fontify white r.collidingWith)

kittenFound : Colliding a -> Bool
kittenFound r = r.collidingWith == kittenDescription

drawRobot : Element
drawRobot = Text.text (fontify gray "[-]   \n(+)=C \n| | \n000 ")

drawHeart : Element
drawHeart = Text.text (fontify red ".::. .::.\n:::::::::\n \':::::\'\n  \':::\'")

drawKitten : Element
drawKitten = Text.text (fontify orange (" |\\_/|\n |0 0|__\n =-*-=  \\\nc_c__(___)"))

foundAnimation : (Int, Int) -> Colliding (Item {}) -> Element
foundAnimation (w,h) robot = 
    collage w h [
      filled black (rect (toFloat w) (toFloat h))
      , toForm (flow right [ drawRobot, drawHeart, drawKitten ]),
      move (nextPoint (robot.xd, robot.yd - 5) (w, h) (Text.text (fontify white robot.char))) 
            (toForm (getMessage robot))
    ]

drawItemForm : Element -> (Int, Int) -> Item a -> Form
drawItemForm roboElem (w, h) item = 
    move (nextPoint (item.xd, item.yd) (w, h) roboElem) 
         (toForm (Text.text (fontify item.cd item.char)))


nextPoint : (Int, Int) -> (Int, Int) -> Element -> (Float, Float)
nextPoint (x, y) (w', h') roboElem =
  let (nextX, nextY) = (toFloat ((widthOf roboElem) * x), toFloat ((heightOf roboElem) * y))
      (w, h) = (toFloat w', toFloat h')
  in 
    if | nextX*2 > w -> (nextX - w, nextY)
       | nextY*2 > h -> (nextX, nextY - h)
       | otherwise -> (nextX, nextY)

render : (Int, Int) -> (Colliding (Item {}), [Item a]) -> Element
render (w, h) (robot, items) =
  let roboElem = Text.text ( fontify white robot.char )
  in case kittenFound robot of
    False -> collage w h ( (++) ([
      filled black (rect (toFloat w) (toFloat h))
      , move (nextPoint (robot.xd, robot.yd - 1) (w, h) roboElem) (toForm (getMessage robot)) 
      , move (nextPoint (robot.xd, robot.yd) (w, h) roboElem) (toForm roboElem)
    ]) (map (drawItemForm roboElem (w,h)) items))
    True -> foundAnimation (w, h) robot

updatePosition : Item a -> (Int, Int) -> Item a
updatePosition r (x, y) = {r | xd <- r.xd + x, yd <- r.yd + y}

removeCollision : Colliding a -> Colliding a
removeCollision r = { r | collidingWith <- ""}

step : {x:Int, y:Int} -> (Colliding(Item {}), [Item {}]) -> (Colliding(Item {}), [Item {}])
step {x, y} (({xd, yd, collidingWith} as r), items) = 
  if x /= 0 || y /= 0 then
    case (collision (updatePosition r (x, y)) items) of
      Just otherItem -> ({ r | collidingWith <- otherItem.description }, items)
      Nothing -> (updatePosition (removeCollision r) (x, y), items)
  else (r, items)

input : Signal {x:Int, y:Int}
input = --let delta = lift (\t -> t/20) (fps 25)
        --in sampleOn delta (lift2 (,) delta Keyboard.arrows)
        Keyboard.arrows

makeGen : Generator.Generator Generator.Standard.Standard
makeGen = Generator.Standard.generator 42

--pass maximum/minimum to this function
--(should bear some resemblance to the wrapping level, 
--otherwise kitten may be tragically rendered offscreen and unreachable)
makeItems : (Int) -> (Int, Int) -> [Item {}]
makeItems numToMake (w, h) =
  let (gen', _, nonKittenItems) = itemify (makeGen, rawItemList, [])
  in (++) ([ { char = "#", description = kittenDescription,
     isKitten = True, xd = 2, yd = 2, cd = orange} ] ) 
    (take numToMake nonKittenItems)

itemify : (Generator.Generator a, [String], [Item {}]) -> (Generator.Generator a, [String], [Item {}])
itemify (gen, descs, items) =
  let (randoms, gen') = Generator.listOf (Generator.int32Range (0,200)) 4 gen 
  in (gen, tail descs, items ++ [{ char = "#", description = (head descs), 
    isKitten = False, xd = head randoms, yd = last (take 2 randoms), cd = blue }])

main
 =
  let items = makeItems 10 (200, 200)
  in lift2 render Window.dimensions (foldp step (robot, items) input)

rawItemList : [ String ]
rawItemList = [
  "\"I pity the fool who mistakes me for kitten!\", sez Mr. T."
  ,"That's just an old tin can."
  ,"It's an altar to the horse god."
  ,"A box of dancing mechanical pencils. They dance! They sing!"
  ,"It's an old Duke Ellington record."
  ,"A box of fumigation pellets."
  ,"A digital clock. It's stuck at 2:17 PM."
  ,"That's just a charred human corpse."
  ,"I don't know what that is, but it's not kitten."
  ,"An empty shopping bag. Paper or plastic?"
  ,"Could it be... a big ugly bowling trophy?"
  ,"A coat hanger hovers in thin air. Odd."
  ,"Not kitten, just a packet of Kool-Aid(tm)."
  ,"A freshly-baked pumpkin pie."
  ,"A lone, forgotten comma, sits here, sobbing."
  ,"ONE HUNDRED THOUSAND CARPET FIBERS!!!!!"
  ,"It's Richard Nixon's nose!"
  ,"It's Lucy Ricardo. \"Aaaah, Ricky!\", she says."
  ,"You stumble upon Bill Gates' stand-up act."
  ,"Just an autographed copy of the Kama Sutra."
  ,"It's the Will Rogers Highway. Who was Will Rogers, anyway?"
  ,"It's another robot, more advanced in design than you but strangely immobile."
  ,"Leonard Richardson is here, asking people to lick him."
  ,"It's a stupid mask, fashioned after a beagle."
  ,"Your State Farm Insurance(tm) representative!"
  ,"It's the local draft board."
  ,"Seven 1/4\"screws and a piece of plastic."
  ,"An 80286 machine."
  ,"A signpost saying \"TO KITTEN\". It points in no particular direction."
  ,"A hammock stretched between a tree and a volleyball pole."
  ,"A Texas Instruments of Destruction calculator."
  ,"It's a dark, amphorous blob of matter."
  ,"Just a pincushion."
  ,"It's a mighty zombie talking about some love and prosperity."
  ,"\"Dear robot, you may have already won our 10 MILLION DOLLAR prize...\""
  ,"It's just an object."
  ,"A mere collection of pixels."
  ,"A badly dented high-hat cymbal lies on its side here."
  ,"A marijuana brownie."
  ,"A plush Chewbacca."
  ,"Daily hunger conditioner from Australasia"
  ,"Just some stuff."
  ,"Why are you touching this when you should be finding kitten?"
  ,"A glorious fan of peacock feathers."
  ,"It's some compromising photos of Babar the Elephant."
  ,"A copy of the Weekly World News. Watch out for the chambered nautilus!"
  ,"It's the proverbial wet blanket."
  ,"Paul Moyer's necktie."
  ,"A haircut and a real job. Now you know where to get one!"
  ,"An automated robot-hater. It frowns disapprovingly at you."
  ,"An automated robot-liker. It smiles at you."
  ,"It's a black hole. Don't fall in!"
  ,"Just a big brick wall."
  ,"You found kitten! No, just kidding."
  ,"Heart of Darkness brand pistachio nuts."
  ,"A smoking branding iron shaped like a 24-pin connector."
  ,"It's a Java applet."
  ,"An abandoned used-car lot."
  ,"A shameless plug for Hacker School: http://www.hackerschool.com"
  ,"A shameless plug for Elm: http://elm-lang.org"
  ,"A can of Spam Lite."
  ,"This is another fine mess you've gotten us into, Stanley."
  ,"It's scenery for \"Waiting for Godot\"."
  ,"This grain elevator towers high above you."
  ,"A Mentos wrapper."
  ,"It's the constellation Pisces."
  ,"It's a fly on the wall. Hi, fly!"
  ,"This kind of looks like kitten, but it's not."
  ,"It's a banana! Oh, joy!"
  ,"A helicopter has crashed here."
  ,"Carlos Tarango stands here, doing his best impression of Pat Smear."
  ,"A patch of mushrooms grows here."
  ,"A patch of grape jelly grows here."
  ,"A spindle, and a grindle, and a bucka-wacka-woom!"
  ,"A geyser sprays water high into the air."
  ,"A toenail? What good is a toenail?"
  ,"You've found the fish! Not that it does you much good in this game."
  ,"A Buttertonsils bar."
  ,"One of the few remaining discoes."
  ,"Ah, the uniform of a Revolutionary-era minuteman."
  ,"A punch bowl, filled with punch and lemon slices."
  ,"It's nothing but a G-thang, baby."
  ,"IT'S ALIVE! AH HA HA HA HA!"
  ,"This was no boating accident!"
  ,"Wait! This isn't the poker chip! You've been tricked! DAMN YOU, MENDEZ!"
  ,"A livery stable! Get your livery!"
  ,"It's a perpetual immobility machine."
  ,"\"On this spot in 1962, Henry Winkler was sick.\""
  ,"There's nothing here; it's just an optical illusion."
  ,"The World's Biggest Motzah Ball!"
  ,"A tribe of cannibals lives here. They eat Malt-O-Meal for breakfast, you know."
  ,"This appears to be a rather large stack of trashy romance novels."
  ,"Look out! Exclamation points!"
  ,"A herd of wild coffee mugs slumbers here."
  ,"It's a limbo bar! How low can you go?"
  ,"It's the horizon. Now THAT'S weird."
  ,"A vase full of artificial flowers is stuck to the floor here."
  ,"A large snake bars your way."
  ,"A pair of saloon-style doors swing slowly back and forth here."
  ,"It's an ordinary bust of Beethoven... but why is it painted green?"
  ,"Hey, look, it's war. What is it good for? Absolutely nothing. Say it again."
  ,"It's the amazing self-referential thing that's not kitten."
  ,"A flamboyant feather boa. Now you can dress up like Carol Channing!"
  ,"This is a large brown bear. Oddly enough, it's currently peeing in the woods."
  ,"A team of arctic explorers is camped here."
  ,"This object here appears to be Louis Farrakhan's bow tie."
  ,"This is the world-famous Chain of Jockstraps."
  ,"A trash compactor, compacting away."
  ,"This toaster strudel is riddled with bullet holes!"
  ,"It's a hologram of a crashed helicopter."
  ,"This is a television. On screen you see a robot strangely similar to yourself."
  ,"This balogna has a first name, it's R-A-N-C-I-D."
  ,"A salmon hatchery? Look again. It's merely a single salmon."
  ,"It's a rim shot. Ba-da-boom!"
  ,"It's creepy and it's kooky, mysterious and spooky. It's also somewhat ooky."
  ,"This is an anagram."
  ,"This object is like an analogy."
  ,"It's a symbol. You see in it a model for all symbols everywhere."
  ,"The object pushes back at you."
  ,"A traffic signal. It appears to have been recently vandalized."
  ,"\"There is no kitten!\"cackles the old crone. You are shocked by her blasphemy."
  ,"This is a Lagrange point. Don't come too close now."
  ,"The dirty old tramp bemoans the loss of his harmonica."
  ,"Look, it's Fanny the Irishman!"
  ,"What in blazes is this?"
  ,"It's the instruction manual for a previous version of this game."
  ,"A brain cell. Oddly enough, it seems to be functioning."
  ,"Tea and/or crumpets."
  ,"This jukebox has nothing but Cliff Richards albums in it."
  ,"It's a Quaker Oatmeal tube, converted into a drum."
  ,"This is a remote control. Being a robot, you keep a wide berth."
  ,"It's a roll of industrial-strength copper wire."
  ,"Oh boy! Grub! Er, grubs."
  ,"A puddle of mud, where the mudskippers play."
  ,"Plenty of nothing."
  ,"Look at that, it's the Crudmobile."
  ,"Just Walter Mattheau and Jack Lemmon."
  ,"Two crepes, two crepes in a box."
  ,"An autographed copy of \"Primary Colors\", by Anonymous."
  ,"Another rabbit? That's three today!"
  ,"It's a segmentation fault. Core dumped, by the way."
  ,"A historical marker showing the actual location of /dev/null."
  ,"Thar's Mobius Dick, the convoluted whale. Arrr!"
  ,"It's a charcoal briquette, smoking away."
  ,"A pizza, melting in the sun."
  ,"It's a \"HOME ALONE 2: Lost in New York\" novelty cup."
  ,"A stack of 7 inch floppies wobbles precariously."
  ,"It's nothing but a corrupted floppy. Coaster anyone?"
  ,"A section of glowing phosphor cells sings a song of radiation to you."
  ,"This TRS-80 III is eerily silent."
  ,"A toilet bowl occupies this space."
  ,"This peg-leg is stuck in a knothole!"
  ,"It's a solitary vacuum tube."
  ,"This corroded robot is clutching a mitten."
  ,"This subwoofer was blown out in 1974."
  ,"Three half-pennies and a wooden nickel."
  ,"This smiling family is happy because they eat LARD."
  ,"Roger Avery, persona un famoso de los Estados Unidos."
  ,"Ne'er but a potted plant."
  ,"A parrot, kipping on its back."
  ,"A forgotten telephone switchboardt a broken hard drive containg the archives of Nerth Pork."
  ,"A broken metronome sits here, it's needle off to one side."
  ,"A sign reads: \"Go home!\""
  ,"A sign reads: \"No robots allowed!\""
  ,"It's the handheld robotfindskitten game, by Tiger."
  ,"This particular monstrosity appears to be ENIAC."
  ,"This is a tasty-looking banana creme pie."
  ,"A wireframe model of a hot dog rotates in space here."
  ,"Just the empty husk of a locust."
  ,"You disturb a murder of crows."
  ,"It's a copy of the robotfindskitten EULA."
  ,"It's Death."
  ,"It's an autographed copy of \"Secondary Colors,\" by Bob Ross."
  ,"It is a marzipan dreadnought that appears to have melted and stuck."
  ,"It's a DVD of \"Crouching Monkey, Hidden Kitten\", region encoded for the moon."
  ,"It's Kieran Hervold.  Damn dyslexia!"
  ,"A non-descript box of crackers."
  ,"Carbonated Water, High Fructose Corn Syrup, Color, Phosphoric Acid, Flavors, Caffeine."
  ,"Move along! Nothing to see here!\""
  ,"It's the embalmed corpse of Vladimir Lenin."
  ,"A coupon for one free steak-fish at your local family diner."
  ,"A set of keys to a 2001 Rolls Royce. Worthless."
  ,"A gravestone stands here.  \"Izchak Miller, ascended.\""
  ,"Someone has written \"ad aerarium\" on the ground here."
  ,"A large blue eye floats in midair."
  ,"This appears to be a statue of Perseus."
  ,"There is an opulent throne here."
  ,"It's a squad of Keystone Kops."
  ,"This seems to be junk mail addressed to the finder of the Eye of Larn."
  ,"A wondrous and intricate golden amulet.  Too bad you have no neck."
  ,"The swampy ground around you seems to stink with disease."
  ,"An animate blob of acid.  Being metallic, you keep well away."
  ,"It's a copy of Knuth with the chapter on kitten-search algorithms torn out."
  ,"A crowd of people, and at the center, a popular misconception."
  ,"It's a blind man. When you touch, he exclaims \"It's a kitten prospecting robot!\""
  ,"It's a lost wallet. It's owner didn't have pets, so you discard it."
  ,"This place is called Antarctica. There is no kitten here."
  ,"It's a mousetrap, baited with soap."
  ,"A book with \"Don't Panic\" in large friendly letters across the cover."
  ,"A compendium of haiku about metals."
  ,"A discredited cosmology, relic of a bygone era."
  ,"A hollow voice says \"Plugh\"."
  ,"A knight who says \"Either I am an insane knave, or you will find kitten.\""
  ,"A neural net -- maybe it's trying to recognize kitten."
  ,"A screwdriver."
  ,"A statue of a girl holding a goose like the one in Gottingen, Germany."
  ,"A tetradrachm dated \"42 B.C.\""
  ,"A voice booms out \"Onward, kitten soldiers...\""
  ,"An eminently forgettable zahir."
  ,"Apparently, it's Edmund Burke."
  ,"For a moment, you feel something in your hands, but it disappears!"
  ,"Here is a book about Robert Kennedy."
  ,"Hey, robot, leave those lists alone."
  ,"Ho hum.  Another synthetic a posteriori."
  ,"It's Asimov's Laws of Robotics.  You feel a strange affinity for them."
  ,"It's Bach's Mass in B-minor!"
  ,"It's a bug."
  ,"It's a synthetic a priori truth!  Immanuel would be so pleased!"
  ,"It's the Tiki Room."
  ,"Just some old play by a Czech playwright, and you can't read Czech."
  ,"Kitten is the letter 'Q'.  Oh, wait, maybe not."
  ,"Quidquid Latine dictum sit, kitten non est."
  ,"Sutro Tower is visible at some distance through the fog."
  ,"The Digital Millennium Copyright Act of 1998."
  ,"The United States Court of Appeals for the Federal Circuit."
  ,"The non-kitten item like this but with \"false\" and \"true\" switched is true."
  ,"The non-kitten item like this but with \"true\" and \"false\" switched is false."
  ,"This is the chapter called \"A Map of the Cat?\" from Feynman's autobiography."
  ,"This is the forest primeval."
  ,"Werner's \"Pocket Field Guide to Things That Are Not Kitten\"."
  ,"You found nettik, but that's backwards."
  ,"You have found some zinc, but you must not stop here, for you must find kitten."
  ,"\"50 Years Among the Non-Kitten Items\", by Ann Droyd."
  ,"\"Robot may not injure kitten, or, through inaction, ...\""
  ,"\"Address Allocation for Private Internets\" by Yakov Rekhter et al."
  ,"\"Mail Routing and the Domain System\" by Craig Partridge."
  ,"\"The Theory and Practice of Oligarchical Collectivism\" by Emmanuel Goldstein.\""
  ,"\"201 Kitten Verbs, Fully Conjugated\".  You look for \"find\"."
  ,"A card shark sits here, practicing his Faro shuffle.  He ignores you."
  ,"A copy of DeCSS.  They're a dime a dozen these days."
  ,"A lotus.  You make an interesting pair."
  ,"A milk carton, with a black and white picture of kitten on the side."
  ,"Any ordinary robot could see from a mile away that this wasn't kitten."
  ,"A stegosaurus, escaped from the stegosaurusfindsrobot game.  It finds you."
  ,"Baling wire and chewing gum."
  ,"Chewing gum and baling wire."
  ,"Here is no kitten but only rock, rock and no kitten and the sandy road."
  ,"Hey, I bet you thought this was kitten."
  ,"It is an ancient mariner, and he stoppeth one of three."
  ,"It pleases you to be kind to what appears to be kitten -- but it's not!"
  ,"It's a blatant plug for Ogg Vorbis, http://www.vorbis.com/"
  ,"It's a business plan for a new startup, kitten.net."
  ,"It's a revised business plan for a new startup, my.kitten.net."
  ,"It's a square."
  ,"It's the Donation of Constantine!"
  ,"It's this message, nothing more."
  ,"Lysine, an essential amino acid.  Well, maybe not for robots."
  ,"No kitten here."
  ,"This looks like Bradley's \"Appearance and Reality\", but it's really not."
  ,"This non-kitten item no verb."
  ,"You feel strangely unfulfilled."
  ,"You hit the non-kitten item.  The non-kitten item fails to yowl."
  ,"You suddenly yearn for your distant homeland."
  ,"You've found the snows of yesteryear!  So that's where they all went to."
  ,"Approaching.  One car.  J.  Followed by.  Two car.  M, M.  In five. Minutes."
  ,"Free Jon Johansen!"
  ,"Free Dmitry Sklyarov!"
  ,"Judith Platt insults librarians."
  ,"This map is not the territory."
  ,"This is a porcelain kitten-counter.  0, 0, 0, 0, 0..."
  ,"An old bootable business card, unfortunately cracked down the middle."
  ,"A kitten sink, for washing kitten (if only kitten liked water)."
  ,"A kitten source (to match the kitten sink)."
  ,"If it's one thing, it's not another."
  ,"If it's not one thing, it's another."
  ,"A caboodle."
  ,"A grin."
  ,"A hedgehog.  It looks like it knows something important."
  ,"You've found... Oh wait, that's just a cat."
  ,"Robot should not be touching that."
  ,"Air Guitar!!!  NA na NA na!!"
  ,"An aromatherapy candle burns with healing light."
  ,"You find a bright shiny penny."
  ,"It's a free Jon Johansen!"
  ,"It's a free Dmitry Sklyarov!"
  ,"The rothe hits!  The rothe hits!"
  ,"It's an Internet chain letter about sodium laureth sulfate."
  ,"Ed Witten sits here, pondering string theory."
  ,"Something is written here in the dust.  You read: \"rJbotf ndQkttten\"."
  ,"We wish you a merry kitten, and a happy New Year!"
  ,"Run away!  Run away!"
  ,"You can see right through this copy of Brin\'s \"Transparent Society\"."
  ,"This copy of \"Steal This Book\"has been stolen from a bookstore."
  ,"It's Roya Naini."
  ,"This kit is the fourteenth in a series of kits named with Roman letters."
  ,"This is the tenth key you've found so far."
  ,"You find a fraud scheme in which loans are used as security for other loans."
  ,"It's the phrase \"and her\", written in ancient Greek."
  ,"It's the author of \"Randomness and Mathematical Proof\"."
  ,"It's the crusty exoskeleton of an arthropod!"
  ,"It's Emporer Shaddam the 4th's planet!"
  ,"It's the triangle leg adjacent to an angle divided by the leg opposite it."
  ,"It's a bottle of nail polish remover."
  ,"You found netkit! Way to go, robot!"
  ,"It's the ASCII Floating Head of Seth David Schoen!"
  ,"A frosted pink party-cake, half eaten."
  ,"A bitchin' homemade tesla coil."
  ,"Conan O'Brian, sans jawbone."
  ,"It's either a mirror, or another soulless kitten-seeking robot."
  ,"Preoccupation with finding kitten prevents you from investigating further."
  ,"Fonzie sits here, mumbling incoherently about a shark and a pair of waterskis."
  ,"The ghost of your dance instructor, his face a paper-white mask of evil."
  ,"A bag of groceries taken off the shelf before the expiration date."
  ,"A book: Feng Shui, Zen: the art of randomly arranging items that are not kitten."
  ,"This might be the fountain of youth, but you'll never know."
  ,"Tigerbot Hesh."
  ,"Stimutacs."
  ,"A canister of pressurized whipped cream, sans whipped cream."
  ,"The non-kitten item bites!"
  ,"A chain hanging from two posts reminds you of the Gateway Arch."
  ,"A mathematician calculates the halting probability of a Turing machine."
  ,"A number of short theatrical productions are indexed 1, 2, 3, ... n."
  ,"A technical university in Australia."
  ,"It is -- I just feel something wonderful is about to happen."
  ,"It's a Cat 5 cable."
  ,"It's a U.S. president."
  ,"It's a piece of cloth used to cover a stage in between performances."
  ,"The ionosphere seems charged with meaning."
  ,"This tomography is like, hella axial, man!"
  ,"It's your favorite game -- robotfindscatan!"
  ,"Just a man selling an albatross." 
  ,"The intermission from a 1930s silent movie."
  ,"It's an inverted billiard ball!"
  ,"The spectre of Sherlock Holmes wills you onwards." ]