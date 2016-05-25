

macro_rules! plus {
  // Nooo this doesn't actually compress things at compiletime
  ( $( $n:expr ),* ) => {
    {
      let mut sum = 0;
      $(
        sum += $n;
      )*
      sum
    }
  };
}

macro_rules! div {
  ( $num:expr , $den:expr ) => {
    // NOPE error comes too late; will be same for vector ops
    { if ($den == 0) { println!("Incoming division error"); };
      $num / $den;
    }
  };
}

fn main() {
  let m = 0;
  //let n = 1 / m; // unchecked
  let n = div!(1, m);
  //println!("result is {}", n);
  return
}
