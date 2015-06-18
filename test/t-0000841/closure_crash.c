string * a = ({"one", "two", "three"});
closure b = (:a:);
closure c = function void : int x {};

void reset()
{
	destruct(this_object());
}
