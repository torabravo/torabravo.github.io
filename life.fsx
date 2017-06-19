open System
open System.Text
open System.IO

type Day = { Number:int; Date:DateTime }

type Year = { Year:int; Days:Day list }

type Decade = { Years:Year list }

type Event = 
    | Day of DateTime 
    | Range of DateTime * DateTime

let daySize = 5
let offset = daySize + 2
let yearOffset = 33


let renderDay day x y =
    let clas = if (day.Number >= 0 && day.Date <= DateTime.Now) then "act" else "psv"
    sprintf "<rect width='%d' height='%d' x='%d' y='%d' id='%d' class='%s'/>" daySize daySize x y day.Number clas


let renderYearRow (days:Day list) y =
    days 
    |> List.mapi (fun i day -> renderDay day (i*offset + yearOffset) y)


let renderYear (year, y) =
    let textOffset = y + 10
    let text = sprintf "<text x='0' y='%d'>%d</text>" textOffset year.Year
    let fstRowDays = (year.Days |> List.take 182 |> renderYearRow) y
    let sndRowDays = (year.Days |> List.skip 182 |> renderYearRow) (y + offset)
    text :: List.concat [fstRowDays; sndRowDays]


let render decades =
    let sb = new StringBuilder()
    sb.Append "<link href='styles.css' rel='stylesheet'/>
    <div>Inspired by <a href='http://waitbutwhy.com/2015/12/the-tail-end.html'>The Tail End</a></div>
    <svg height=1200 width=1400>" |> ignore
    
    let mutable y = 0;
    dec
    for dec in decades do
        for year in dec.Years do
            renderYear (year, y) |> List.iter(fun s -> sb.Append(s) |> ignore)
            y <- y + offset*2
        y <- y + offset

    sb.ToString() + "</svg>"    


let getData (born:DateTime) lifeExpectancy =
    let die = born.AddYears(lifeExpectancy)
    new DateTime(born.Year, 1, 1)
    |> Array.unfold (fun (day:DateTime) -> if day >= die then None else Some (day, day.AddDays(1.0)))
    |> Array.mapi (fun i d-> { Number=(if d >= born then i else -1); Date=d})
    |> Array.groupBy (fun d -> d.Date.Year)
    |> Array.map (fun (year, days) -> { Year=year; Days=days |> List.ofSeq })
    |> Array.chunkBySize (10)
    |> Array.map (fun years -> {Years = List.ofArray years})


let born = new DateTime (1984,9,30)
let data = getData born 80 |> List.ofSeq
let str = render data
let path = Path.Combine(__SOURCE_DIRECTORY__, "index.html")
File.WriteAllText(path, str)