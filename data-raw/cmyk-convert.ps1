# Converts CMYK recipes through the US Web Coated (SWOP) v2 ICC profile, the
# profile used for the transcribed values, into
#   * an sRGB hex code (clipped to what screens can show), and
#   * the CIELAB (D50) colour as printed, via ProPhoto RGB, whose gamut holds
#     every SWOP colour. The build uses it to measure how far each hex falls
#     short of the print.
# Both reproduce the transcribed values: hex to within 4/255 per channel, print
# Lab to within 0.5 CIEDE2000. Windows only: uses WPF colour management and the
# profiles that ship in the Windows colour directory.
#
#   powershell -File data-raw/cmyk-convert.ps1 100 90 38 50
#   powershell -File data-raw/cmyk-convert.ps1 -Csv data-raw/colours.csv   # fills blank cells

param(
  [Parameter(Position = 0)][double]$C,
  [Parameter(Position = 1)][double]$M,
  [Parameter(Position = 2)][double]$Y,
  [Parameter(Position = 3)][double]$K,
  [string]$Csv
)

Add-Type -AssemblyName PresentationCore, WindowsBase
$dir = "$env:WINDIR\System32\spool\drivers\color"
$swop = New-Object Uri "$dir\USWebCoatedSWOP.icc"
$prophoto = New-Object Uri "$dir\ProPhoto.icm"

function ConvertTo-Hex([double]$c, [double]$m, [double]$y, [double]$k) {
  $ink = [single[]]@(($c / 100), ($m / 100), ($y / 100), ($k / 100))
  $col = [System.Windows.Media.Color]::FromValues($ink, $swop)
  "#{0:X2}{1:X2}{2:X2}" -f $col.R, $col.G, $col.B
}

function ConvertTo-PrintLab([double]$c, [double]$m, [double]$y, [double]$k) {
  $px = [byte[]]@([math]::Round($c * 2.55), [math]::Round($m * 2.55), [math]::Round($y * 2.55), [math]::Round($k * 2.55))
  $src = [System.Windows.Media.Imaging.BitmapSource]::Create(1, 1, 96, 96, [System.Windows.Media.PixelFormats]::Cmyk32, $null, $px, 4)
  $conv = New-Object System.Windows.Media.Imaging.ColorConvertedBitmap ($src,
    (New-Object System.Windows.Media.ColorContext $swop), (New-Object System.Windows.Media.ColorContext $prophoto),
    [System.Windows.Media.PixelFormats]::Rgba64)
  $out = New-Object uint16[] 4
  $conv.CopyPixels($out, 8, 0)
  # ProPhoto RGB (gamma 1.8, D50) -> XYZ -> Lab
  $lin = 0..2 | ForEach-Object { [math]::Pow($out[$_] / 65535, 1.8) }
  $x = 0.7976749 * $lin[0] + 0.1351917 * $lin[1] + 0.0313534 * $lin[2]
  $yy = 0.2880402 * $lin[0] + 0.7118741 * $lin[1] + 0.0000857 * $lin[2]
  $z = 0.8252100 * $lin[2]
  $f = { param($t) if ($t -gt [math]::Pow(6 / 29, 3)) { [math]::Pow($t, 1 / 3) } else { $t / (3 * [math]::Pow(6 / 29, 2)) + 4 / 29 } }
  $fx = & $f ($x / 0.9642); $fy = & $f $yy; $fz = & $f ($z / 0.8249)
  @([math]::Round(116 * $fy - 16, 2), [math]::Round(500 * ($fx - $fy), 2), [math]::Round(200 * ($fy - $fz), 2))
}

if ($Csv) {
  $rows = Import-Csv $Csv
  foreach ($r in $rows) {
    if (-not $r.hex) {
      $r.hex = ConvertTo-Hex $r.cyan $r.magenta $r.yellow $r.black
      Write-Host "$($r.name): hex $($r.hex)"
    }
    if (-not $r.print_lab_l) {
      $lab = ConvertTo-PrintLab $r.cyan $r.magenta $r.yellow $r.black
      $r.print_lab_l = $lab[0]; $r.print_lab_a = $lab[1]; $r.print_lab_b = $lab[2]
      Write-Host "$($r.name): print Lab $($lab -join ' ')"
    }
  }
  $rows | Export-Csv $Csv -NoTypeInformation -Encoding UTF8
} else {
  $lab = ConvertTo-PrintLab $C $M $Y $K
  "hex $(ConvertTo-Hex $C $M $Y $K)  print Lab(D50) $($lab -join ' ')"
}
