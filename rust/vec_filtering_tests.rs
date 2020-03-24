fn main() {
    let vec : Vec<i32> = vec![0, 1, 2, 3, 2, 4, 2, 5];
    let filtered_vec : Vec<i32> = vec.iter().filter(|&&i| i != 2).cloned().collect();
    let mut retained_vec : Vec<i32> = vec.clone();
    retained_vec.retain(|&i| i != 2);
    println!("Vec: {:?}\nFiltered vec: {:?}\nRetained vec: {:?}", vec, filtered_vec, retained_vec);
}