function Get-Shares(){
     param (
             [string] $ServerName
     )
     $out =@()
     $netResp = (net view $ServerName)[7..100]
     foreach($line in $netResp){

         if($line -match "the command completed.*"){
             break;
         }
         $out += $line.Replace(" Disk","").Trim()
     }
     return $out;
}
