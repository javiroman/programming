module.exports = function () {
   return {
       categories: ["Watersports", "Soccer", "Chess"],
       products: [
           { id:1, name: "Kayak", category: "Watersports",
               description: "A boat for one person", price: 275},
           { id:2, name: "LifeJacket", category: "Watersports",
               description: "Protective and chulo", price: 48.95},
           { id:3, name: "Soccer Ball", category: "Soccer",
               description: "Pelota ofical liga", price: 19.50},
           { id:4, name: "Corner Flags", category: "Soccer",
               description: "Give your playing field a professional touch",
               price: 34.95}
       ],
    orders: []
   }
}