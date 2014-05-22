{-
 - All raw data in here is copied from: 
 - http://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/RIFF.html
 -}

-- | The Data section of WAVE files can contain many different types of encoded data. It
-- can contain everytihng from raw PCM data (which this library uses as it's base
-- encoding) to a development encoding. The encoding that was used is saved in the Format
-- Chunk of the WAVE file and this module is responsible for making that one piece of data
-- easier to understand and deal with. We provide a number of helper functions to make the
-- AudioFormat easier to understand.
module Sound.Wav.AudioFormats 
   ( AudioFormatData
   , AudioFormat(..)
   , getAudioFormat
   , putAudioFormat
   , prettyShowAudioFormat
   )
 where

import Data.Word
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

import qualified Data.Map as M

-- | A representation of the AudioFormatData as seen in the Format Section in the header
-- of a WAVE file.
type AudioFormatData = Word16

-- | Extracts the actual AudioFormat from the raw AudioFormatData
getAudioFormat 
   :: AudioFormatData -- ^ The raw audio format data, likely taken from the header of a WAVE file.
   -> AudioFormat     -- ^ The actual AudioFormat as understood by wavy.
getAudioFormat key = fromMaybe defaultValue possibleKey  
   where
      possibleKey = M.lookup key audioFormatMap
      defaultValue = UnknownFormat key

-- | A conversion function from a known AudioFormat to it's representation as
-- AudioFormatData. This function is useful if you are planning on writing out your WAVE
-- data and sharing it.
putAudioFormat 
   :: AudioFormat     -- ^ The audio format you wish to convert.
   -> AudioFormatData -- ^ The raw representation of the Audio Format.
putAudioFormat (UnknownFormat v) = v
putAudioFormat key = fromMaybe defaultValue possibleKey
   where
      possibleKey = M.lookup key reverseAudioFormatMap
      -- Unknown Code
      defaultValue = 0xe708

reverseAudioFormatMap :: M.Map AudioFormat AudioFormatData
reverseAudioFormatMap = M.fromList $ fmap swap audioCodecs

audioFormatMap :: M.Map AudioFormatData AudioFormat
audioFormatMap = M.fromList audioCodecs

audioCodecs :: [(AudioFormatData, AudioFormat)]
audioCodecs = 
   [ (0x1, MicrosoftPCM)
   , (0x2, MicrosoftADPCM)
   , (0x3, MicrosoftIEEEfloat)
   , (0x4, CompaqVSELP)
   , (0x5, IBMCVSD)
   , (0x6, MicrosoftaLaw)
   , (0x7, MicrosoftuLaw)
   , (0x8, MicrosoftDTS)
   , (0x9, DRM)
   , (0xa, WMA9Speech)
   , (0xb, MicrosoftWindowsMediaRTVoice)
   , (0x10, OKIADPCM)
   , (0x11, IntelIMADVIADPCM)
   , (0x12, VideologicMediaspaceADPCM)
   , (0x13, SierraADPCM)
   , (0x14, AntexG723ADPCM)
   , (0x15, DSPSolutionsDIGISTD)
   , (0x16, DSPSolutionsDIGIFIX)
   , (0x17, DialoicOKIADPCM)
   , (0x18, MediaVisionADPCM)
   , (0x19, HPCU)
   , (0x1a, HPDynamicVoice)
   , (0x20, YamahaADPCM)
   , (0x21, SONARCSpeechCompression)
   , (0x22, DSPGroupTrueSpeech)
   , (0x23, EchoSpeechCorp)
   , (0x24, VirtualMusicAudiofileAF36)
   , (0x25, AudioProcessingTech)
   , (0x26, VirtualMusicAudiofileAF10)
   , (0x27, AculabProsody1612)
   , (0x28, MergingTechLRC)
   , (0x30, DolbyAC2)
   , (0x31, MicrosoftGSM610)
   , (0x32, MSNAudio)
   , (0x33, AntexADPCME)
   , (0x34, ControlResourcesVQLPC)
   , (0x35, DSPSolutionsDIGIREAL)
   , (0x36, DSPSolutionsDIGIADPCM)
   , (0x37, ControlResourcesCR10)
   , (0x38, NaturalMicroSystemsVBXADPCM)
   , (0x39, CrystalSemiconductorIMAADPCM)
   , (0x3a, EchoSpeechECHOSC3)
   , (0x3b, RockwellADPCM)
   , (0x3c, RockwellDIGITALK)
   , (0x3d, XebecMultimedia)
   , (0x40, AntexG721ADPCM)
   , (0x41, AntexG728CELP)
   , (0x42, MicrosoftMSG723)
   , (0x43, IBMAVCADPCM)
   , (0x45, ITUTG726)
   , (0x50, MicrosoftMPEG)
   , (0x51, RT23orPAC)
   , (0x52, InSoftRT24)
   , (0x53, InSoftPAC)
   , (0x55, MP3)
   , (0x59, Cirrus)
   , (0x60, CirrusLogic)
   , (0x61, ESSTechPCM)
   , (0x62, VoxwareInc)
   , (0x63, CanopusATRAC)
   , (0x64, APICOMG726ADPCM)
   , (0x65, APICOMG722ADPCM)
   , (0x66, MicrosoftDSAT)
   , (0x67, MicorsoftDSATDISPLAY)
   , (0x69, VoxwareByteAligned)
   , (0x70, VoxwareAC8)
   , (0x71, VoxwareAC10)
   , (0x72, VoxwareAC16)
   , (0x73, VoxwareAC20)
   , (0x74, VoxwareMetaVoice)
   , (0x75, VoxwareMetaSound)
   , (0x76, VoxwareRT29HW)
   , (0x77, VoxwareVR12)
   , (0x78, VoxwareVR18)
   , (0x79, VoxwareTQ40)
   , (0x7a, VoxwareSC3)
   , (0x7b, VoxwareSC3)
   , (0x80, Soundsoft)
   , (0x81, VoxwareTQ60)
   , (0x82, MicrosoftMSRT24)
   , (0x83, ATandTG729A)
   , (0x84, MotionPixelsMVIMV12)
   , (0x85, DataFusionG726)
   , (0x86, DataFusionGSM610)
   , (0x88, IteratedSystemsAudio)
   , (0x89, Onlive)
   , (0x8a, MultitudeIncFTSX20)
   , (0x8b, InfocomITSASG721ADPCM)
   , (0x8c, ConvediaG729)
   , (0x8d, NotspecifiedcongruencyInc)
   , (0x91, SiemensSBC24)
   , (0x92, SonicFoundryDolbyAC3APDIF)
   , (0x93, MediaSonicG723)
   , (0x94, AculabProsody8kbps)
   , (0x97, ZyXELADPCM)
   , (0x98, PhilipsLPCBB)
   , (0x99, StuderProfessionalAudioPacked)
   , (0xa0, MaldenPhonyTalk)
   , (0xa1, RacalRecorderGSM)
   , (0xa2, RacalRecorderG720a)
   , (0xa3, RacalG7231)
   , (0xa4, RacalTetraACELP)
   , (0xb0, NECAACNECCorporation)
   , (0xff, AAC)
   , (0x100, RhetorexADPCM)
   , (0x101, IBMuLaw)
   , (0x102, IBMaLaw)
   , (0x103, IBMADPCM)
   , (0x111, VivoG723)
   , (0x112, VivoSiren)
   , (0x120, PhilipsSpeechProcessingCELP)
   , (0x121, PhilipsSpeechProcessingGRUNDIG)
   , (0x123, DigitalG723)
   , (0x125, SanyoLDADPCM)
   , (0x130, SiproLabACEPLNET)
   , (0x131, SiproLabACELP4800)
   , (0x132, SiproLabACELP8V3)
   , (0x133, SiproLabG729)
   , (0x134, SiproLabG729A)
   , (0x135, SiproLabKelvin)
   , (0x136, VoiceAgeAMR)
   , (0x140, DictaphoneG726ADPCM)
   , (0x150, QualcommPureVoice)
   , (0x151, QualcommHalfRate)
   , (0x155, RingZeroSystemsTUBGSM)
   , (0x160, MicrosoftAudio1)
   , (0x161, WindowsMediaAudioV2V7V8V9DivXaudioSpecWMAAlexAC3Audio)
   , (0x162, WindowsMediaAudioProfessionalV9)
   , (0x163, WindowsMediaAudioLosslessV9)
   , (0x164, WMAProoverSPDIF)
   , (0x170, UNISYSNAPADPCM)
   , (0x171, UNISYSNAPULAW)
   , (0x172, UNISYSNAPALAW)
   , (0x173, UNISYSNAP16K)
   , (0x174, MMSYCOMACMSYC008SyComTechnologies)
   , (0x175, MMSYCOMACMSYC701G726LSyComTechnologies)
   , (0x176, MMSYCOMACMSYC701CELP54SyComTechnologies)
   , (0x177, MMSYCOMACMSYC701CELP68SyComTechnologies)
   , (0x178, KnowledgeAdventureADPCM)
   , (0x180, FraunhoferIISMPEG2AAC)
   , (0x190, DigitalTheaterSystemsDTSDS)
   , (0x200, CreativeLabsADPCM)
   , (0x202, CreativeLabsFASTSPEECH8)
   , (0x203, CreativeLabsFASTSPEECH10)
   , (0x210, UHERADPCM)
   , (0x215, UleadDVACM)
   , (0x216, UleadDVACM)
   , (0x220, QuarterdeckCorp)
   , (0x230, ILinkVC)
   , (0x240, AurealSemiconductorRawSport)
   , (0x241, ESSTAC3)
   , (0x250, InteractiveProductsHSX)
   , (0x251, InteractiveProductsRPELP)
   , (0x260, ConsistentCS2)
   , (0x270, SonySCX)
   , (0x271, SonySCY)
   , (0x272, SonyATRAC3)
   , (0x273, SonySPC)
   , (0x280, TELUMTelumInc)
   , (0x281, TELUMIATelumInc)
   , (0x285, NorcomVoiceSystemsADPCM)
   , (0x300, FujitsuFMTOWNSSND)
   , (0x301, FujitsuSpecnotspecified)
   , (0x302, FujitsuSpecnotspecified)
   , (0x303, FujitsuSpecnotspecified)
   , (0x304, FujitsuSpecnotspecified)
   , (0x305, FujitsuSpecnotspecified)
   , (0x306, FujitsuSpecnotspecified)
   , (0x307, FujitsuSpecnotspecified)
   , (0x308, FujitsuSpecnotspecified)
   , (0x350, MicronasSemiconductorsIncDevelopment)
   , (0x351, MicronasSemiconductorsIncCELP833)
   , (0x400, BrooktreeDigital)
   , (0x401, IntelMusicCoderSpecIMC)
   , (0x402, LigosIndeoAudio)
   , (0x450, QDesignMusic)
   , (0x500, On2VP7On2Technologies)
   , (0x501, On2VP6On2Technologies)
   , (0x680, ATandTVMEVMPCM)
   , (0x681, ATandTTCP)
   , (0x700, YMPEGAlphaSpecdummyforMPEG2compressor)
   , (0x8ae, ClearJumpLiteWaveSpeclossless)
   , (0x1000, OlivettiGSM)
   , (0x1001, OlivettiADPCM)
   , (0x1002, OlivettiCELP)
   , (0x1003, OlivettiSBC)
   , (0x1004, OlivettiOPR)
   , (0x1100, LernoutandHauspie)
   , (0x1101, LernoutandHauspieCELPcodec)
   , (0x1102, LernoutandHauspieSBCcodec)
   , (0x1103, LernoutandHauspieSBCcodec)
   , (0x1104, LernoutandHauspieSBCcodec)
   , (0x1400, NorrisCommInc)
   , (0x1401, ISIAudio)
   , (0x1500, ATandTSoundspaceMusicCompression)
   , (0x181c, VoxWareRT24speechcodec)
   , (0x181e, LucentelemediaAX24000PMusiccodec)
   , (0x1971, SonicFoundryLOSSLESS)
   , (0x1979, InningsTelecomIncADPCM)
   , (0x1c07, LucentSX8300Pspeechcodec)
   , (0x1c0c, LucentSX5363SG723compliantcodec)
   , (0x1f03, CUseeMeDigiTalkSpecexRocwell)
   , (0x1fc4, NCTSoftALF2CDACM)
   , (0x2000, FASTMultimediaDVM)
   , (0x2001, DolbyDTSSpecDigitalTheaterSystem)
   , (0x2002, RealAudio12144)
   , (0x2003, RealAudio1Slash2288)
   , (0x2004, RealAudioG2Slash8CookSpeclowbitrate)
   , (0x2005, RealAudio3Slash4Slash5MusicSpecDNET)
   , (0x2006, RealAudio10AACSpecRAAC)
   , (0x2007, RealAudio10AACPlusSpecRACP)
   , (0x2500, Reservedrangeto0x2600Microsoft)
   , (0x3313, MakeAVISSpecffvfwfakeAVIsoundfromAviSynthscripts)
   , (0x4143, DivioMPEG4AACaudio)
   , (0x4201, Nokiaadaptivemultirate)
   , (0x4243, DivioG726DivioInc)
   , (0x434c, LEADSpeech)
   , (0x564c, LEADVorbis)
   , (0x5756, WavPackAudio)
   , (0x674f, OggVorbisSpecmode1)
   , (0x6750, OggVorbisSpecmode2)
   , (0x6751, OggVorbisSpecmode3)
   , (0x676f, OggVorbisSpecmode1Plus)
   , (0x6770, OggVorbisSpecmode2Plus)
   , (0x6771, OggVorbisSpecmode3Plus)
   , (0x7000, ThreeCOMNBX3ComCorporation)
   , (0x706d, FAADAAC)
   , (0x7a21, GSMAMRSpecCBRnoSID)
   , (0x7a22, GSMAMRSpecVBRincludingSID)
   , (0xa100, ComverseInfosysLtdG7231)
   , (0xa101, ComverseInfosysLtdAVQSBC)
   , (0xa102, ComverseInfosysLtdOLDSBC)
   , (0xa103, SymbolTechnologiesG729A)
   , (0xa104, VoiceAgeAMRWBVoiceAgeCorporation)
   , (0xa105, IngenientTechnologiesIncG726)
   , (0xa106, ISOSlashMPEG4advancedaudioCoding)
   , (0xa107, EncoreSoftwareLtdG726)
   , (0xa109, SpeexACMCodecxiphorg)
   , (0xdfac, DebugModeSonicFoundryVegasFrameServerACMCodec)
   , (0xe708, Unknown)
   , (0xf1ac, FreeLosslessAudioCodecFLAC)
   , (0xfffe, Extensible)
   , (0xffff, Development)
   ]

-- | This is a massive data structure that contains every single different audio format
-- that I could find. It allows us to represent AudioFormats in a very human readable and
-- easy to understand manner insize wavy. This data structure will be constantly growing
-- and changing. Do not be surprised if names change or elements are added. Because this
-- list is always growing we have an UnknownFormat element that can wrap and
-- AudioFormatData so that, in the event that we do not know about an audio format. You
-- can still deal with it naturally.
data AudioFormat
   = MicrosoftPCM
   | MicrosoftADPCM
   | MicrosoftIEEEfloat
   | CompaqVSELP
   | IBMCVSD
   | MicrosoftaLaw
   | MicrosoftuLaw
   | MicrosoftDTS
   | DRM
   | WMA9Speech
   | MicrosoftWindowsMediaRTVoice
   | OKIADPCM
   | IntelIMADVIADPCM
   | VideologicMediaspaceADPCM
   | SierraADPCM
   | AntexG723ADPCM
   | DSPSolutionsDIGISTD
   | DSPSolutionsDIGIFIX
   | DialoicOKIADPCM
   | MediaVisionADPCM
   | HPCU
   | HPDynamicVoice
   | YamahaADPCM
   | SONARCSpeechCompression
   | DSPGroupTrueSpeech
   | EchoSpeechCorp
   | VirtualMusicAudiofileAF36
   | AudioProcessingTech
   | VirtualMusicAudiofileAF10
   | AculabProsody1612
   | MergingTechLRC
   | DolbyAC2
   | MicrosoftGSM610
   | MSNAudio
   | AntexADPCME
   | ControlResourcesVQLPC
   | DSPSolutionsDIGIREAL
   | DSPSolutionsDIGIADPCM
   | ControlResourcesCR10
   | NaturalMicroSystemsVBXADPCM
   | CrystalSemiconductorIMAADPCM
   | EchoSpeechECHOSC3
   | RockwellADPCM
   | RockwellDIGITALK
   | XebecMultimedia
   | AntexG721ADPCM
   | AntexG728CELP
   | MicrosoftMSG723
   | IBMAVCADPCM
   | ITUTG726
   | MicrosoftMPEG
   | RT23orPAC
   | InSoftRT24
   | InSoftPAC
   | MP3
   | Cirrus
   | CirrusLogic
   | ESSTechPCM
   | VoxwareInc
   | CanopusATRAC
   | APICOMG726ADPCM
   | APICOMG722ADPCM
   | MicrosoftDSAT
   | MicorsoftDSATDISPLAY
   | VoxwareByteAligned
   | VoxwareAC8
   | VoxwareAC10
   | VoxwareAC16
   | VoxwareAC20
   | VoxwareMetaVoice
   | VoxwareMetaSound
   | VoxwareRT29HW
   | VoxwareVR12
   | VoxwareVR18
   | VoxwareTQ40
   | VoxwareSC3
   | Soundsoft
   | VoxwareTQ60
   | MicrosoftMSRT24
   | ATandTG729A
   | MotionPixelsMVIMV12
   | DataFusionG726
   | DataFusionGSM610
   | IteratedSystemsAudio
   | Onlive
   | MultitudeIncFTSX20
   | InfocomITSASG721ADPCM
   | ConvediaG729
   | NotspecifiedcongruencyInc
   | SiemensSBC24
   | SonicFoundryDolbyAC3APDIF
   | MediaSonicG723
   | AculabProsody8kbps
   | ZyXELADPCM
   | PhilipsLPCBB
   | StuderProfessionalAudioPacked
   | MaldenPhonyTalk
   | RacalRecorderGSM
   | RacalRecorderG720a
   | RacalG7231
   | RacalTetraACELP
   | NECAACNECCorporation
   | AAC
   | RhetorexADPCM
   | IBMuLaw
   | IBMaLaw
   | IBMADPCM
   | VivoG723
   | VivoSiren
   | PhilipsSpeechProcessingCELP
   | PhilipsSpeechProcessingGRUNDIG
   | DigitalG723
   | SanyoLDADPCM
   | SiproLabACEPLNET
   | SiproLabACELP4800
   | SiproLabACELP8V3
   | SiproLabG729
   | SiproLabG729A
   | SiproLabKelvin
   | VoiceAgeAMR
   | DictaphoneG726ADPCM
   | QualcommPureVoice
   | QualcommHalfRate
   | RingZeroSystemsTUBGSM
   | MicrosoftAudio1
   | WindowsMediaAudioV2V7V8V9DivXaudioSpecWMAAlexAC3Audio
   | WindowsMediaAudioProfessionalV9
   | WindowsMediaAudioLosslessV9
   | WMAProoverSPDIF
   | UNISYSNAPADPCM
   | UNISYSNAPULAW
   | UNISYSNAPALAW
   | UNISYSNAP16K
   | MMSYCOMACMSYC008SyComTechnologies
   | MMSYCOMACMSYC701G726LSyComTechnologies
   | MMSYCOMACMSYC701CELP54SyComTechnologies
   | MMSYCOMACMSYC701CELP68SyComTechnologies
   | KnowledgeAdventureADPCM
   | FraunhoferIISMPEG2AAC
   | DigitalTheaterSystemsDTSDS
   | CreativeLabsADPCM
   | CreativeLabsFASTSPEECH8
   | CreativeLabsFASTSPEECH10
   | UHERADPCM
   | UleadDVACM
   | QuarterdeckCorp
   | ILinkVC
   | AurealSemiconductorRawSport
   | ESSTAC3
   | InteractiveProductsHSX
   | InteractiveProductsRPELP
   | ConsistentCS2
   | SonySCX
   | SonySCY
   | SonyATRAC3
   | SonySPC
   | TELUMTelumInc
   | TELUMIATelumInc
   | NorcomVoiceSystemsADPCM
   | FujitsuFMTOWNSSND
   | FujitsuSpecnotspecified
   | MicronasSemiconductorsIncDevelopment
   | MicronasSemiconductorsIncCELP833
   | BrooktreeDigital
   | IntelMusicCoderSpecIMC
   | LigosIndeoAudio
   | QDesignMusic
   | On2VP7On2Technologies
   | On2VP6On2Technologies
   | ATandTVMEVMPCM
   | ATandTTCP
   | YMPEGAlphaSpecdummyforMPEG2compressor
   | ClearJumpLiteWaveSpeclossless
   | OlivettiGSM
   | OlivettiADPCM
   | OlivettiCELP
   | OlivettiSBC
   | OlivettiOPR
   | LernoutandHauspie
   | LernoutandHauspieCELPcodec
   | LernoutandHauspieSBCcodec
   | NorrisCommInc
   | ISIAudio
   | ATandTSoundspaceMusicCompression
   | VoxWareRT24speechcodec
   | LucentelemediaAX24000PMusiccodec
   | SonicFoundryLOSSLESS
   | InningsTelecomIncADPCM
   | LucentSX8300Pspeechcodec
   | LucentSX5363SG723compliantcodec
   | CUseeMeDigiTalkSpecexRocwell
   | NCTSoftALF2CDACM
   | FASTMultimediaDVM
   | DolbyDTSSpecDigitalTheaterSystem
   | RealAudio12144
   | RealAudio1Slash2288
   | RealAudioG2Slash8CookSpeclowbitrate
   | RealAudio3Slash4Slash5MusicSpecDNET
   | RealAudio10AACSpecRAAC
   | RealAudio10AACPlusSpecRACP
   | Reservedrangeto0x2600Microsoft
   | MakeAVISSpecffvfwfakeAVIsoundfromAviSynthscripts
   | DivioMPEG4AACaudio
   | Nokiaadaptivemultirate
   | DivioG726DivioInc
   | LEADSpeech
   | LEADVorbis
   | WavPackAudio
   | OggVorbisSpecmode1
   | OggVorbisSpecmode2
   | OggVorbisSpecmode3
   | OggVorbisSpecmode1Plus
   | OggVorbisSpecmode2Plus
   | OggVorbisSpecmode3Plus
   | ThreeCOMNBX3ComCorporation
   | FAADAAC
   | GSMAMRSpecCBRnoSID
   | GSMAMRSpecVBRincludingSID
   | ComverseInfosysLtdG7231
   | ComverseInfosysLtdAVQSBC
   | ComverseInfosysLtdOLDSBC
   | SymbolTechnologiesG729A
   | VoiceAgeAMRWBVoiceAgeCorporation
   | IngenientTechnologiesIncG726
   | ISOSlashMPEG4advancedaudioCoding
   | EncoreSoftwareLtdG726
   | SpeexACMCodecxiphorg
   | DebugModeSonicFoundryVegasFrameServerACMCodec
   | Unknown
   | FreeLosslessAudioCodecFLAC
   | Extensible
   | Development
   | UnknownFormat AudioFormatData
   deriving(Show, Eq, Ord)

-- | This function is reponsible for converting an AudioFormat into a representation that
-- is human readable and what people would expect. It is useful if you need to display
-- your AudioFormat to a human and you want to know, quickly, what to call it.
prettyShowAudioFormat 
   :: AudioFormat -- ^ The audio format that you wish to display to a human.
   -> String      -- ^ The human readable description of this audio format.
prettyShowAudioFormat (UnknownFormat formatNumber) = 
   "Unknown Audio Format (Code: " ++ show formatNumber ++ ")"
prettyShowAudioFormat xs = fromMaybe (noPretty xs) $ M.lookup xs audioFormatShow
   where
      noPretty :: AudioFormat -> String
      noPretty = ("Raw Format: " ++) . show

audioFormatShow :: M.Map AudioFormat String
audioFormatShow = M.fromList
   [ (MicrosoftPCM, "Microsoft PCM")
   , (MicrosoftADPCM, "Microsoft ADPCM")
   , (MicrosoftIEEEfloat, "Microsoft IEEE float")
   , (CompaqVSELP, "Compaq VSELP")
   , (IBMCVSD, "IBM CVSD")
   , (MicrosoftaLaw, "Microsoft a-Law")
   , (MicrosoftuLaw, "Microsoft u-Law")
   , (MicrosoftDTS, "Microsoft DTS")
   , (DRM, "DRM")
   , (WMA9Speech, "WMA 9 Speech")
   , (MicrosoftWindowsMediaRTVoice, "Microsoft Windows Media RT Voice")
   , (OKIADPCM, "OKI-ADPCM")
   , (IntelIMADVIADPCM, "Intel IMA/DVI-ADPCM")
   , (VideologicMediaspaceADPCM, "Videologic Mediaspace ADPCM")
   , (SierraADPCM, "Sierra ADPCM")
   , (AntexG723ADPCM, "Antex G.723 ADPCM")
   , (DSPSolutionsDIGISTD, "DSP Solutions DIGISTD")
   , (DSPSolutionsDIGIFIX, "DSP Solutions DIGIFIX")
   , (DialoicOKIADPCM, "Dialoic OKI ADPCM")
   , (MediaVisionADPCM, "Media Vision ADPCM")
   , (HPCU, "HP CU")
   , (HPDynamicVoice, "HP Dynamic Voice")
   , (YamahaADPCM, "Yamaha ADPCM")
   , (SONARCSpeechCompression, "SONARC Speech Compression")
   , (DSPGroupTrueSpeech, "DSP Group True Speech")
   , (EchoSpeechCorp, "Echo Speech Corp.")
   , (VirtualMusicAudiofileAF36, "Virtual Music Audiofile AF36")
   , (AudioProcessingTech, "Audio Processing Tech.")
   , (VirtualMusicAudiofileAF10, "Virtual Music Audiofile AF10")
   , (AculabProsody1612, "Aculab Prosody 1612")
   , (MergingTechLRC, "Merging Tech. LRC")
   , (DolbyAC2, "Dolby AC2")
   , (MicrosoftGSM610, "Microsoft GSM610")
   , (MSNAudio, "MSN Audio")
   , (AntexADPCME, "Antex ADPCME")
   , (ControlResourcesVQLPC, "Control Resources VQLPC")
   , (DSPSolutionsDIGIREAL, "DSP Solutions DIGIREAL")
   , (DSPSolutionsDIGIADPCM, "DSP Solutions DIGIADPCM")
   , (ControlResourcesCR10, "Control Resources CR10")
   , (NaturalMicroSystemsVBXADPCM, "Natural MicroSystems VBX ADPCM")
   , (CrystalSemiconductorIMAADPCM, "Crystal Semiconductor IMA ADPCM")
   , (EchoSpeechECHOSC3, "Echo Speech ECHOSC3")
   , (RockwellADPCM, "Rockwell ADPCM")
   , (RockwellDIGITALK, "Rockwell DIGITALK")
   , (XebecMultimedia, "Xebec Multimedia")
   , (AntexG721ADPCM, "Antex G.721 ADPCM")
   , (AntexG728CELP, "Antex G.728 CELP")
   , (MicrosoftMSG723, "Microsoft MSG723")
   , (IBMAVCADPCM, "IBM AVC ADPCM")
   , (ITUTG726, "ITU-T G.726")
   , (MicrosoftMPEG, "Microsoft MPEG")
   , (RT23orPAC, "RT23 or PAC")
   , (InSoftRT24, "InSoft RT24")
   , (InSoftPAC, "InSoft PAC")
   , (MP3, "MP3")
   , (Cirrus, "Cirrus")
   , (CirrusLogic, "Cirrus Logic")
   , (ESSTechPCM, "ESS Tech. PCM")
   , (VoxwareInc, "Voxware Inc.")
   , (CanopusATRAC, "Canopus ATRAC")
   , (APICOMG726ADPCM, "APICOM G.726 ADPCM")
   , (APICOMG722ADPCM, "APICOM G.722 ADPCM")
   , (MicrosoftDSAT, "Microsoft DSAT")
   , (MicorsoftDSATDISPLAY, "Micorsoft DSAT DISPLAY")
   , (VoxwareByteAligned, "Voxware Byte Aligned")
   , (VoxwareAC8, "Voxware AC8")
   , (VoxwareAC10, "Voxware AC10")
   , (VoxwareAC16, "Voxware AC16")
   , (VoxwareAC20, "Voxware AC20")
   , (VoxwareMetaVoice, "Voxware MetaVoice")
   , (VoxwareMetaSound, "Voxware MetaSound")
   , (VoxwareRT29HW, "Voxware RT29HW")
   , (VoxwareVR12, "Voxware VR12")
   , (VoxwareVR18, "Voxware VR18")
   , (VoxwareTQ40, "Voxware TQ40")
   , (VoxwareSC3, "Voxware SC3")
   , (VoxwareSC3, "Voxware SC3")
   , (Soundsoft, "Soundsoft")
   , (VoxwareTQ60, "Voxware TQ60")
   , (MicrosoftMSRT24, "Microsoft MSRT24")
   , (ATandTG729A, "AT&T G.729A")
   , (MotionPixelsMVIMV12, "Motion Pixels MVI MV12")
   , (DataFusionG726, "DataFusion G.726")
   , (DataFusionGSM610, "DataFusion GSM610")
   , (IteratedSystemsAudio, "Iterated Systems Audio")
   , (Onlive, "Onlive")
   , (MultitudeIncFTSX20, "Multitude, Inc. FT SX20")
   , (InfocomITSASG721ADPCM, "Infocom ITS A/S G.721 ADPCM")
   , (ConvediaG729, "Convedia G729")
   , (NotspecifiedcongruencyInc, "Not specified congruency, Inc.")
   , (SiemensSBC24, "Siemens SBC24")
   , (SonicFoundryDolbyAC3APDIF, "Sonic Foundry Dolby AC3 APDIF")
   , (MediaSonicG723, "MediaSonic G.723")
   , (AculabProsody8kbps, "Aculab Prosody 8kbps")
   , (ZyXELADPCM, "ZyXEL ADPCM")
   , (PhilipsLPCBB, "Philips LPCBB")
   , (StuderProfessionalAudioPacked, "Studer Professional Audio Packed")
   , (MaldenPhonyTalk, "Malden PhonyTalk")
   , (RacalRecorderGSM, "Racal Recorder GSM")
   , (RacalRecorderG720a, "Racal Recorder G720.a")
   , (RacalG7231, "Racal G723.1")
   , (RacalTetraACELP, "Racal Tetra ACELP")
   , (NECAACNECCorporation, "NEC AAC NEC Corporation")
   , (AAC, "AAC")
   , (RhetorexADPCM, "Rhetorex ADPCM")
   , (IBMuLaw, "IBM u-Law")
   , (IBMaLaw, "IBM a-Law")
   , (IBMADPCM, "IBM ADPCM")
   , (VivoG723, "Vivo G.723")
   , (VivoSiren, "Vivo Siren")
   , (PhilipsSpeechProcessingCELP, "Philips Speech Processing CELP")
   , (PhilipsSpeechProcessingGRUNDIG, "Philips Speech Processing GRUNDIG")
   , (DigitalG723, "Digital G.723")
   , (SanyoLDADPCM, "Sanyo LD ADPCM")
   , (SiproLabACEPLNET, "Sipro Lab ACEPLNET")
   , (SiproLabACELP4800, "Sipro Lab ACELP4800")
   , (SiproLabACELP8V3, "Sipro Lab ACELP8V3")
   , (SiproLabG729, "Sipro Lab G.729")
   , (SiproLabG729A, "Sipro Lab G.729A")
   , (SiproLabKelvin, "Sipro Lab Kelvin")
   , (VoiceAgeAMR, "VoiceAge AMR")
   , (DictaphoneG726ADPCM, "Dictaphone G.726 ADPCM")
   , (QualcommPureVoice, "Qualcomm PureVoice")
   , (QualcommHalfRate, "Qualcomm HalfRate")
   , (RingZeroSystemsTUBGSM, "Ring Zero Systems TUBGSM")
   , (MicrosoftAudio1, "Microsoft Audio1")
   , (WindowsMediaAudioV2V7V8V9DivXaudioSpecWMAAlexAC3Audio, "Windows Media Audio V2 V7 V8 V9 / DivX audio (WMA) / Alex AC3 Audio")
   , (WindowsMediaAudioProfessionalV9, "Windows Media Audio Professional V9")
   , (WindowsMediaAudioLosslessV9, "Windows Media Audio Lossless V9")
   , (WMAProoverSPDIF, "WMA Pro over S/PDIF")
   , (UNISYSNAPADPCM, "UNISYS NAP ADPCM")
   , (UNISYSNAPULAW, "UNISYS NAP ULAW")
   , (UNISYSNAPALAW, "UNISYS NAP ALAW")
   , (UNISYSNAP16K, "UNISYS NAP 16K")
   , (MMSYCOMACMSYC008SyComTechnologies, "MM SYCOM ACM SYC008 SyCom Technologies")
   , (MMSYCOMACMSYC701G726LSyComTechnologies, "MM SYCOM ACM SYC701 G726L SyCom Technologies")
   , (MMSYCOMACMSYC701CELP54SyComTechnologies, "MM SYCOM ACM SYC701 CELP54 SyCom Technologies")
   , (MMSYCOMACMSYC701CELP68SyComTechnologies, "MM SYCOM ACM SYC701 CELP68 SyCom Technologies")
   , (KnowledgeAdventureADPCM, "Knowledge Adventure ADPCM")
   , (FraunhoferIISMPEG2AAC, "Fraunhofer IIS MPEG2AAC")
   , (DigitalTheaterSystemsDTSDS, "Digital Theater Systems DTS DS")
   , (CreativeLabsADPCM, "Creative Labs ADPCM")
   , (CreativeLabsFASTSPEECH8, "Creative Labs FASTSPEECH8")
   , (CreativeLabsFASTSPEECH10, "Creative Labs FASTSPEECH10")
   , (UHERADPCM, "UHER ADPCM")
   , (UleadDVACM, "Ulead DV ACM")
   , (UleadDVACM, "Ulead DV ACM")
   , (QuarterdeckCorp, "Quarterdeck Corp.")
   , (ILinkVC, "I-Link VC")
   , (AurealSemiconductorRawSport, "Aureal Semiconductor Raw Sport")
   , (ESSTAC3, "ESST AC3")
   , (InteractiveProductsHSX, "Interactive Products HSX")
   , (InteractiveProductsRPELP, "Interactive Products RPELP")
   , (ConsistentCS2, "Consistent CS2")
   , (SonySCX, "Sony SCX")
   , (SonySCY, "Sony SCY")
   , (SonyATRAC3, "Sony ATRAC3")
   , (SonySPC, "Sony SPC")
   , (TELUMTelumInc, "TELUM Telum Inc.")
   , (TELUMIATelumInc, "TELUMIA Telum Inc.")
   , (NorcomVoiceSystemsADPCM, "Norcom Voice Systems ADPCM")
   , (FujitsuFMTOWNSSND, "Fujitsu FM TOWNS SND")
   , (FujitsuSpecnotspecified, "Fujitsu (not specified)")
   , (MicronasSemiconductorsIncDevelopment, "Micronas Semiconductors, Inc. Development")
   , (MicronasSemiconductorsIncCELP833, "Micronas Semiconductors, Inc. CELP833")
   , (BrooktreeDigital, "Brooktree Digital")
   , (IntelMusicCoderSpecIMC, "Intel Music Coder (IMC)")
   , (LigosIndeoAudio, "Ligos Indeo Audio")
   , (QDesignMusic, "QDesign Music")
   , (On2VP7On2Technologies, "On2 VP7 On2 Technologies")
   , (On2VP6On2Technologies, "On2 VP6 On2 Technologies")
   , (ATandTVMEVMPCM, "AT&T VME VMPCM")
   , (ATandTTCP, "AT&T TCP")
   , (YMPEGAlphaSpecdummyforMPEG2compressor, "YMPEG Alpha (dummy for MPEG-2 compressor)")
   , (ClearJumpLiteWaveSpeclossless, "ClearJump LiteWave (lossless)")
   , (OlivettiGSM, "Olivetti GSM")
   , (OlivettiADPCM, "Olivetti ADPCM")
   , (OlivettiCELP, "Olivetti CELP")
   , (OlivettiSBC, "Olivetti SBC")
   , (OlivettiOPR, "Olivetti OPR")
   , (LernoutandHauspie, "Lernout & Hauspie")
   , (LernoutandHauspieCELPcodec, "Lernout & Hauspie CELP codec")
   , (LernoutandHauspieSBCcodec, "Lernout & Hauspie SBC codec")
   , (LernoutandHauspieSBCcodec, "Lernout & Hauspie SBC codec")
   , (LernoutandHauspieSBCcodec, "Lernout & Hauspie SBC codec")
   , (NorrisCommInc, "Norris Comm. Inc.")
   , (ISIAudio, "ISIAudio")
   , (ATandTSoundspaceMusicCompression, "AT&T Soundspace Music Compression")
   , (VoxWareRT24speechcodec, "VoxWare RT24 speech codec")
   , (LucentelemediaAX24000PMusiccodec, "Lucent elemedia AX24000P Music codec")
   , (SonicFoundryLOSSLESS, "Sonic Foundry LOSSLESS")
   , (InningsTelecomIncADPCM, "Innings Telecom Inc. ADPCM")
   , (LucentSX8300Pspeechcodec, "Lucent SX8300P speech codec")
   , (LucentSX5363SG723compliantcodec, "Lucent SX5363S G.723 compliant codec")
   , (CUseeMeDigiTalkSpecexRocwell, "CUseeMe DigiTalk (ex-Rocwell)")
   , (NCTSoftALF2CDACM, "NCT Soft ALF2CD ACM")
   , (FASTMultimediaDVM, "FAST Multimedia DVM")
   , (DolbyDTSSpecDigitalTheaterSystem, "Dolby DTS (Digital Theater System)")
   , (RealAudio12144, "RealAudio 1 / 2 14.4")
   , (RealAudio1Slash2288, "RealAudio 1 / 2 28.8")
   , (RealAudioG2Slash8CookSpeclowbitrate, "RealAudio G2 / 8 Cook (low bitrate)")
   , (RealAudio3Slash4Slash5MusicSpecDNET, "RealAudio 3 / 4 / 5 Music (DNET)")
   , (RealAudio10AACSpecRAAC, "RealAudio 10 AAC (RAAC)")
   , (RealAudio10AACPlusSpecRACP, "RealAudio 10 AAC+ (RACP)")
   , (Reservedrangeto0x2600Microsoft, "Reserved range to 0x2600 Microsoft")
   , (MakeAVISSpecffvfwfakeAVIsoundfromAviSynthscripts, "makeAVIS (ffvfw fake AVI sound from AviSynth scripts)")
   , (DivioMPEG4AACaudio, "Divio MPEG-4 AAC audio")
   , (Nokiaadaptivemultirate, "Nokia adaptive multirate")
   , (DivioG726DivioInc, "Divio G726 Divio, Inc.")
   , (LEADSpeech, "LEAD Speech")
   , (LEADVorbis, "LEAD Vorbis")
   , (WavPackAudio, "WavPack Audio")
   , (OggVorbisSpecmode1, "Ogg Vorbis (mode 1)")
   , (OggVorbisSpecmode2, "Ogg Vorbis (mode 2)")
   , (OggVorbisSpecmode3, "Ogg Vorbis (mode 3)")
   , (OggVorbisSpecmode1Plus, "Ogg Vorbis (mode 1+)")
   , (OggVorbisSpecmode2Plus, "Ogg Vorbis (mode 2+)")
   , (OggVorbisSpecmode3Plus, "Ogg Vorbis (mode 3+)")
   , (ThreeCOMNBX3ComCorporation, "3COM NBX 3Com Corporation")
   , (FAADAAC, "FAAD AAC")
   , (GSMAMRSpecCBRnoSID, "GSM-AMR (CBR, no SID)")
   , (GSMAMRSpecVBRincludingSID, "GSM-AMR (VBR, including SID)")
   , (ComverseInfosysLtdG7231, "Comverse Infosys Ltd. G723 1")
   , (ComverseInfosysLtdAVQSBC, "Comverse Infosys Ltd. AVQSBC")
   , (ComverseInfosysLtdOLDSBC, "Comverse Infosys Ltd. OLDSBC")
   , (SymbolTechnologiesG729A, "Symbol Technologies G729A")
   , (VoiceAgeAMRWBVoiceAgeCorporation, "VoiceAge AMR WB VoiceAge Corporation")
   , (IngenientTechnologiesIncG726, "Ingenient Technologies Inc. G726")
   , (ISOSlashMPEG4advancedaudioCoding, "ISO/MPEG-4 advanced audio Coding")
   , (EncoreSoftwareLtdG726, "Encore Software Ltd G726")
   , (SpeexACMCodecxiphorg, "Speex ACM Codec xiph.org")
   , (DebugModeSonicFoundryVegasFrameServerACMCodec, "DebugMode SonicFoundry Vegas FrameServer ACM Codec")
   , (Unknown, "Unknown -")
   , (FreeLosslessAudioCodecFLAC, "Free Lossless Audio Codec FLAC")
   , (Extensible, "Extensible")
   , (Development, "Development")
   ]
