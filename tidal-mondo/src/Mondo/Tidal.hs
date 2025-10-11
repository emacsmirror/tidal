{-# LANGUAGE ImportQualifiedPost #-}

{- | This module regroups the tidal functions by their types.
The collection are named after the function's arguments, separated by '_':
* pA is 'Pattern a'
* pC is 'ControlPattern'
* pStr is 'Pattern String'
* pInt is 'Pattern Int'
* time is 'Time'

Examples:
* pStr_pC is 'Pattern String -> ControlPattern'
* time_pC_pC is 'Time -> ControlPattern -> ControlPattern'

Note: perhaps this should be part of tidal-core?
-}
module Mondo.Tidal where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Sound.Tidal.Control qualified as T
import Sound.Tidal.Core qualified as T
import Sound.Tidal.Params qualified as T
import Sound.Tidal.Pattern qualified as T
import Sound.Tidal.Stepwise qualified as T
import Sound.Tidal.UI qualified as T

pStr_pC :: Map String (T.Pattern String -> T.ControlPattern)
pStr_pC =
    Map.fromList
        [ ("sound", T.sound)
        , ("cc", T.cc)
        , ("nrpn", T.nrpn)
        , ("grain'", T.grain')
        , ("drum", T.drum)
        , ("bank", T.bank)
        , ("midicmd", T.midicmd)
        , ("toArg", T.toArg)
        , ("unit", T.unit)
        , ("vowel", T.vowel)
        , ("s", T.s)
        ]

int_pInt :: Map String (Int -> T.Pattern Int)
int_pInt =
    Map.fromList
        [ ("randrun", T.randrun)
        ]

pInt_pNum :: (Num a) => Map String (T.Pattern Int -> T.Pattern a)
pInt_pNum =
    Map.fromList
        [ ("irand", T.irand)
        ]

pENum_pENum :: (Enum a, Num a) => Map String (T.Pattern a -> T.Pattern a)
pENum_pENum =
    Map.fromList
        [ ("scan", T.scan)
        ]

pFrac :: (Fractional a) => Map String (T.Pattern a)
pFrac =
    Map.fromList
        [ ("sine", T.sine)
        , ("square", T.square)
        , ("cosine", T.cosine)
        , ("rand", T.rand)
        , ("perlin", T.perlin)
        ]

pENR_pENR :: (Enum a, Num a, Real a) => Map String (T.Pattern a -> T.Pattern a)
pENR_pENR =
    Map.fromList
        [ ("run", T.run)
        ]

pFracReal :: (Fractional a, Real a) => Map String (T.Pattern a)
pFracReal =
    Map.fromList
        [ ("saw", T.saw)
        , ("tri", T.tri)
        ]

pA_pA :: Map String (T.Pattern a -> T.Pattern a)
pA_pA =
    Map.fromList
        [ ("trigger", T.trigger)
        , ("qtrigger", T.qtrigger)
        , ("qt", T.qt)
        , ("ctrigger", T.ctrigger)
        , ("rtrigger", T.rtrigger)
        , ("ftrigger", T.ftrigger)
        , ("mono", T.mono)
        , ("splitQueries", T.splitQueries)
        , ("rev", T.rev)
        , ("filterOnsets", T.filterOnsets)
        , ("filterDigital", T.filterDigital)
        , ("filterAnalog", T.filterAnalog)
        , ("degrade", T.degrade)
        , ("brak", T.brak)
        , ("palindrome", T.palindrome)
        , ("stretch", T.stretch)
        , ("loopFirst", T.loopFirst)
        , ("arpeggiate", T.arpeggiate)
        , ("arpg", T.arpg)
        , ("rolled", T.rolled)
        , ("press", T.press)
        ]

pTime_pTime_pC_pC :: Map String (T.Pattern T.Time -> T.Pattern T.Time -> T.ControlPattern -> T.ControlPattern)
pTime_pTime_pC_pC =
    Map.fromList
        [ ("ribbon", T.ribbon)
        ]

pC_pC :: Map String (T.ControlPattern -> T.ControlPattern)
pC_pC =
    Map.fromList
        [ ("ghost", T.ghost)
        ]

time_pC_pC :: Map String (T.Time -> T.ControlPattern -> T.ControlPattern)
time_pC_pC =
    Map.fromList
        [ ("ghost'", T.ghost')
        , ("_pressBy", T._pressBy)
        ]

-- sometimes and often are not strictly for control pattern, but it's simpler to restrict them here.
pCpC_pC_pC :: Map String ((T.ControlPattern -> T.ControlPattern) -> T.ControlPattern -> T.ControlPattern)
pCpC_pC_pC =
    Map.fromList
        [ ("sometimes", T.sometimes)
        , ("often", T.often)
        , ("jux", T.jux)
        ]

pTime_pA_pA :: Map String (T.Pattern T.Time -> T.Pattern a -> T.Pattern a)
pTime_pA_pA =
    Map.fromList
        [ ("slowSqueeze", T.slowSqueeze)
        , ("sparsity", T.sparsity)
        , ("fastGap", T.fastGap)
        , ("densityGap", T.densityGap)
        , ("fast", T.fast)
        , ("fastSqueeze", T.fastSqueeze)
        , ("density", T.density)
        , ("slow", T.slow)
        , ("steptake", T.steptake)
        , ("stepdrop", T.stepdrop)
        , ("trunc", T.trunc)
        , ("linger", T.linger)
        , ("segment", T.segment)
        , ("discretise", T.discretise)
        , ("timeLoop", T.timeLoop)
        , ("swing", T.swing)
        , ("pressBy", T.pressBy)
        ]

pBool_pA_pA :: Map String (T.Pattern Bool -> T.Pattern a -> T.Pattern a)
pBool_pA_pA =
    Map.fromList
        [ ("reset", T.reset)
        , ("restart", T.restart)
        , ("struct", T.struct)
        , ("mask", T.mask)
        ]

pInt_pA_pA :: Map String (T.Pattern Int -> T.Pattern a -> T.Pattern a)
pInt_pA_pA =
    Map.fromList
        [ ("repeatCycles", T.repeatCycles)
        , ("iter", T.iter)
        , ("iter'", T.iter')
        , ("substruct'", T.substruct')
        , ("stripe", T.stripe)
        , ("slowstripe", T.slowstripe)
        , ("shuffle", T.shuffle)
        , ("scramble", T.scramble)
        ]

pInt_pInt_pC_pC :: Map String (T.Pattern Int -> T.Pattern Int -> T.ControlPattern -> T.ControlPattern)
pInt_pInt_pC_pC =
    Map.fromList
        [ ("splice", T.splice)
        , ("euclid", T.euclid)
        , ("euclidInv", T.euclidInv)
        , ("slice", T.slice)
        , ("chew", T.chew)
        ]

-- render list with: `grep -r tidal-core ":: Pattern Double -> ControlPattern" | sed 's/.*.hs:\([^ ]+\).*/  , ("\1", T.\1)/'`
pDouble_pC :: Map String (T.Pattern Double -> T.ControlPattern)
pDouble_pC =
    Map.fromList
        [ ("accelerate", T.accelerate)
        , ("amp", T.amp)
        , ("attack", T.attack)
        , ("bandf", T.bandf)
        , ("bandq", T.bandq)
        , ("begin", T.begin)
        , ("binshift", T.binshift)
        , ("ccn", T.ccn)
        , ("ccv", T.ccv)
        , ("clhatdecay", T.clhatdecay)
        , ("coarse", T.coarse)
        , ("comb", T.comb)
        , ("control", T.control)
        , ("cps", T.cps)
        , ("crush", T.crush)
        , ("ctlNum", T.ctlNum)
        , ("ctranspose", T.ctranspose)
        , ("cutoff", T.cutoff)
        , ("cutoffegint", T.cutoffegint)
        , ("decay", T.decay)
        , ("degree", T.degree)
        , ("delay", T.delay)
        , ("delayfeedback", T.delayfeedback)
        , ("delaytime", T.delaytime)
        , ("detune", T.detune)
        , ("distort", T.distort)
        , ("djf", T.djf)
        , ("dry", T.dry)
        , ("dur", T.dur)
        , ("end", T.end)
        , ("enhance", T.enhance)
        , ("expression", T.expression)
        , ("fadeInTime", T.fadeInTime)
        , ("fadeTime", T.fadeTime)
        , ("frameRate", T.frameRate)
        , ("frames", T.frames)
        , ("freeze", T.freeze)
        , ("freq", T.freq)
        , ("from", T.from)
        , ("fshift", T.fshift)
        , ("fshiftnote", T.fshiftnote)
        , ("fshiftphase", T.fshiftphase)
        , ("gain", T.gain)
        , ("gate", T.gate)
        , ("harmonic", T.harmonic)
        , ("hatgrain", T.hatgrain)
        , ("hbrick", T.hbrick)
        , ("hcutoff", T.hcutoff)
        , ("hold", T.hold)
        , ("hours", T.hours)
        , ("hresonance", T.hresonance)
        , ("imag", T.imag)
        , ("kcutoff", T.kcutoff)
        , ("krush", T.krush)
        , ("lagogo", T.lagogo)
        , ("lbrick", T.lbrick)
        , ("lclap", T.lclap)
        , ("lclaves", T.lclaves)
        , ("lclhat", T.lclhat)
        , ("lcrash", T.lcrash)
        , ("legato", T.legato)
        , ("clip", T.clip)
        , ("leslie", T.leslie)
        , ("lfo", T.lfo)
        , ("lfocutoffint", T.lfocutoffint)
        , ("lfodelay", T.lfodelay)
        , ("lfoint", T.lfoint)
        , ("lfopitchint", T.lfopitchint)
        , ("lfoshape", T.lfoshape)
        , ("lfosync", T.lfosync)
        , ("lhitom", T.lhitom)
        , ("lkick", T.lkick)
        , ("llotom", T.llotom)
        , ("lock", T.lock)
        , ("loop", T.loop)
        , ("lophat", T.lophat)
        , ("lrate", T.lrate)
        , ("lsize", T.lsize)
        , ("lsnare", T.lsnare)
        , ("metatune", T.metatune)
        , ("midibend", T.midibend)
        , ("midichan", T.midichan)
        , ("miditouch", T.miditouch)
        , ("minutes", T.minutes)
        , ("modwheel", T.modwheel)
        , ("mtranspose", T.mtranspose)
        , ("nudge", T.nudge)
        , ("octaveR", T.octaveR)
        , ("octer", T.octer)
        , ("octersub", T.octersub)
        , ("octersubsub", T.octersubsub)
        , ("offset", T.offset)
        , ("ophatdecay", T.ophatdecay)
        , ("overgain", T.overgain)
        , ("overshape", T.overshape)
        , ("pan", T.pan)
        , ("panorient", T.panorient)
        , ("panspan", T.panspan)
        , ("pansplay", T.pansplay)
        , ("panwidth", T.panwidth)
        , ("partials", T.partials)
        , ("phaserdepth", T.phaserdepth)
        , ("phaserrate", T.phaserrate)
        , ("pitch1", T.pitch1)
        , ("pitch2", T.pitch2)
        , ("pitch3", T.pitch3)
        , ("polyTouch", T.polyTouch)
        , ("portamento", T.portamento)
        , ("progNum", T.progNum)
        , ("rate", T.rate)
        , ("real", T.real)
        , ("release", T.release)
        , ("resonance", T.resonance)
        , ("ring", T.ring)
        , ("ringdf", T.ringdf)
        , ("ringf", T.ringf)
        , ("room", T.room)
        , ("sagogo", T.sagogo)
        , ("sclap", T.sclap)
        , ("sclaves", T.sclaves)
        , ("scram", T.scram)
        , ("scrash", T.scrash)
        , ("seconds", T.seconds)
        , ("semitone", T.semitone)
        , ("shape", T.shape)
        , ("size", T.size)
        , ("slide", T.slide)
        , ("smear", T.smear)
        , ("songPtr", T.songPtr)
        , ("speed", T.speed)
        , ("squiz", T.squiz)
        , ("stepsPerOctave", T.stepsPerOctave)
        , ("stutterdepth", T.stutterdepth)
        , ("stuttertime", T.stuttertime)
        , ("sustain", T.sustain)
        , ("sustainpedal", T.sustainpedal)
        , ("timescale", T.timescale)
        , ("timescalewin", T.timescalewin)
        , ("to", T.to)
        , ("tomdecay", T.tomdecay)
        , ("tremolodepth", T.tremolodepth)
        , ("tremolorate", T.tremolorate)
        , ("triode", T.triode)
        , ("tsdelay", T.tsdelay)
        , ("uid", T.uid)
        , ("val", T.val)
        , ("vcfegint", T.vcfegint)
        , ("vcoegint", T.vcoegint)
        , ("velocity", T.velocity)
        , ("voice", T.voice)
        , ("waveloss", T.waveloss)
        , ("xsdelay", T.xsdelay)
        , ("voi", T.voi)
        , ("vco", T.vco)
        , ("vcf", T.vcf)
        , ("tremr", T.tremr)
        , ("tremdp", T.tremdp)
        , ("tdecay", T.tdecay)
        , ("sz", T.sz)
        , ("sus", T.sus)
        , ("stt", T.stt)
        , ("std", T.std)
        , ("sld", T.sld)
        , ("scr", T.scr)
        , ("scp", T.scp)
        , ("scl", T.scl)
        , ("sag", T.sag)
        , ("rel", T.rel)
        , ("por", T.por)
        , ("pit3", T.pit3)
        , ("pit2", T.pit2)
        , ("pit1", T.pit1)
        , ("phasr", T.phasr)
        , ("phasdp", T.phasdp)
        , ("ohdecay", T.ohdecay)
        , ("lsn", T.lsn)
        , ("lpq", T.lpq)
        , ("lpf", T.lpf)
        , ("loh", T.loh)
        , ("llt", T.llt)
        , ("lht", T.lht)
        , ("lfop", T.lfop)
        , ("lfoi", T.lfoi)
        , ("lfoc", T.lfoc)
        , ("lcr", T.lcr)
        , ("lcp", T.lcp)
        , ("lcl", T.lcl)
        , ("lch", T.lch)
        , ("lbd", T.lbd)
        , ("lag", T.lag)
        , ("hpq", T.hpq)
        , ("hpf", T.hpf)
        , ("hg", T.hg)
        , ("gat", T.gat)
        , ("fadeOutTime", T.fadeOutTime)
        , ("dt", T.dt)
        , ("dfb", T.dfb)
        , ("det", T.det)
        , ("delayt", T.delayt)
        , ("delayfb", T.delayfb)
        , ("ctfg", T.ctfg)
        , ("ctf", T.ctf)
        , ("chdecay", T.chdecay)
        , ("bpq", T.bpq)
        , ("bpf", T.bpf)
        , ("att", T.att)
        , -- strudel aliases
          ("dec", T.decay)
        , ("lpf", T.cutoff)
        , ("hpf", T.hcutoff)
        ]

-- render list with: `grep -r tidal-core ":: Pattern Int -> ControlPattern$" | grep -v "recv" | sed 's/.*.hs:\([^ ]*\).*/  , ("\1", T.\1)/'`
pInt_pC :: Map String (T.Pattern Int -> T.ControlPattern)
pInt_pC =
    Map.fromList
        [ ("nrpnn", T.nrpnn)
        , ("nrpnv", T.nrpnv)
        , ("channel", T.channel)
        , ("cut", T.cut)
        , ("octave", T.octave)
        , ("orbit", T.orbit)
        ]
