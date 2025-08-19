main = do
    let inventory = [("Discos", 30, 72), ("Guitarras", 120, 27)]
    let updatedInventory = addProduct inventory "Repuestos" 7 40
    putStrLn $ "Primer inventario: " ++ show updatedInventory
    let updatedInventory2 = updateQuantity updatedInventory "Discos" 94
    putStrLn $ "Segundo inventario: " ++ show updatedInventory2
    let finalInventory = removeProduct updatedInventory2 "Guitarras"
    putStrLn $ "Inventario final: " ++ show finalInventory
    let (totalQty, totalValue) = inventorySummary finalInventory
    putStrLn $ "Cantidad en stock: " ++ show totalQty
    putStrLn $ "Valor total de los productos: " ++ show totalValue


type Product   = (String, Double, Int)
type Inventory = [Product]

--n,p,q significa name, price y quantity

addProduct :: Inventory -> String -> Double -> Int -> Inventory
addProduct inventory name price quantity =
    inventory ++ [(name, price, quantity)]
updateQuantity :: Inventory -> String -> Int -> Inventory
updateQuantity [] _ _ = []  
updateQuantity ((n,p,q):xs) name newQuantity
    | n == name = (n, p, newQuantity) : xs
    | otherwise = (n, p, q) : updateQuantity xs name newQuantity 
removeProduct :: Inventory -> String -> Inventory
removeProduct [] _ = []
removeProduct ((n,p,q):xs) name
    | n == name = xs                               
    | otherwise = (n,p,q) : removeProduct xs name  
inventorySummary :: Inventory -> (Int, Double)
inventorySummary [] = (0,0) 
inventorySummary ((_,p,q):xs) =
    let (totalQty, totalValue) = inventorySummary xs
    in (q + totalQty, (p * fromIntegral q) + totalValue)