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

let renderYears (years:Year list) y =
    years
    |> List.mapi (fun i year -> renderYear(year, i*offset*2 + y))
    |> List.concat

let render years =   
    let sb = new StringBuilder()
    renderYears years 0 |> List.iter(fun s -> sb.Append(s) |> ignore)
    "<link href='styles.css' rel='stylesheet'/>" +
    "<h1><a href='http://waitbutwhy.com/2015/12/the-tail-end.html'>The Tail End</a></h1>" +
    "<svg height=1200 width=1400>" + sb.ToString() + "</svg>"


let getAllDays (first:DateTime) (last:DateTime) =
    new DateTime(first.Year, 1, 1)
    |> Array.unfold (fun (day:DateTime) -> if day >= last then None else Some (day, day.AddDays(1.0)))
    |> Array.mapi (fun i d-> { Number=(if d >= first then i else -1); Date=d})


let getData () =
    let born = new DateTime (1984,9,30)
    let die = born.AddYears(80)
    getAllDays born die
    |> Array.groupBy (fun d -> d.Date.Year) 
    |> Array.map (fun (year, days) -> { Year=year; Days=days |> List.ofSeq })


let years = getData() |> List.ofSeq
let str = render years
let path = Path.Combine(__SOURCE_DIRECTORY__, "index.html")
File.WriteAllText(path, str)