data Cat = Persian | Siamese | Munchkin
data Dog = Labrador | Pug | Chiuahua

type Pet = Either Cat Dog = : Left Cat | Right Cat 