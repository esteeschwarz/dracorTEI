<?xml version="1.0" encoding="UTF-8"?>
<schema xmlns="http://purl.oclc.org/dsdl/schematron" queryBinding="xslt2">
   <title>ISO Schematron rules (DraCor Schema 1.0.0-2-63f5570)</title>
   <!-- This file generated 2025-08-21T15:17:31Z by 'extract-isosch.xsl'. -->
   <!-- ********************* -->
   <!-- namespaces, declared: -->
   <!-- ********************* -->
   <ns prefix="tei" uri="http://www.tei-c.org/ns/1.0"/>
   <ns prefix="xs" uri="http://www.w3.org/2001/XMLSchema"/>
   <ns prefix="rng" uri="http://relaxng.org/ns/structure/1.0"/>
   <ns prefix="rna" uri="http://relaxng.org/ns/compatibility/annotations/1.0"/>
   <ns prefix="sch" uri="http://purl.oclc.org/dsdl/schematron"/>
   <ns prefix="sch1x" uri="http://www.ascc.net/xml/schematron"/>
   <ns prefix="tei" uri="http://www.tei-c.org/ns/1.0"/>
   <ns prefix="xs" uri="http://www.w3.org/2001/XMLSchema"/>
   <ns prefix="rng" uri="http://relaxng.org/ns/structure/1.0"/>
   <ns prefix="rna" uri="http://relaxng.org/ns/compatibility/annotations/1.0"/>
   <ns prefix="sch" uri="http://purl.oclc.org/dsdl/schematron"/>
   <ns prefix="sch1x" uri="http://www.ascc.net/xml/schematron"/>
   <!-- ******************************************************* -->
   <!-- constraints in en, und, mul, zxx, of which there are 64 -->
   <!-- ******************************************************* -->
   <pattern id="schematron-constraint-CMC_generatedBy_within_post-1">
      <rule context="tei:*[@generatedBy]">
         <assert test="ancestor-or-self::tei:post">The @generatedBy attribute is for use within a &lt;post&gt; element.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-att-datable-w3c-when-2">
      <rule context="tei:*[@when]">
         <report test="@notBefore|@notAfter|@from|@to" role="nonfatal">The @when attribute cannot be used with any other att.datable.w3c attributes.</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-att-datable-w3c-from-3">
      <rule context="tei:*[@from]">
         <report test="@notBefore" role="nonfatal">The @from and @notBefore attributes cannot be used together.</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-att-datable-w3c-to-4">
      <rule context="tei:*[@to]">
         <report test="@notAfter" role="nonfatal">The @to and @notAfter attributes cannot be used together.</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-only_1_ODD_source-5">
      <rule context="tei:*[@source]">
         <let name="srcs" value="tokenize( normalize-space(@source),' ')"/>
         <report test="(   self::tei:classRef                                 | self::tei:dataRef                                 | self::tei:elementRef                                 | self::tei:macroRef                                 | self::tei:moduleRef                                 | self::tei:schemaSpec )                                   and                                   $srcs[2]"> When used on a schema description element (like <value-of select="name(.)"/>), the @source attribute should have only 1 value. (This one has <value-of select="count($srcs)"/>.)</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-att-measurement-unitRef-6">
      <rule context="tei:*[@unitRef]">
         <report test="@unit" role="info">The @unit attribute may be unnecessary when @unitRef is present.</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-targetLang-7">
      <rule context="tei:*[not(self::tei:schemaSpec)][@targetLang]">
         <assert test="@target">@targetLang should only be used on <name/> if @target is specified.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-spanTo-points-to-following-8">
      <rule context="tei:*[ starts-with( @spanTo, '#') ]">
         <assert test="id( substring( @spanTo, 2 ) ) &gt;&gt; ."> The element indicated by @spanTo (<value-of select="@spanTo"/>) must follow the current <name/> element</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-schemeVersionRequiresScheme-9">
      <rule context="tei:*[@schemeVersion]">
         <assert test="@scheme and not(@scheme = 'free')"> @schemeVersion can only be used if @scheme is specified.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-subtypeTyped-10">
      <rule context="tei:*[@subtype]">
         <assert test="@type">The <name/> element should not be categorized in detail with @subtype unless also categorized in general with @type</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-calendar_attr_on_empty_element-11">
      <rule context="tei:*[@calendar]">
         <assert test="string-length( normalize-space(.) ) gt 0"> @calendar indicates one or more systems or calendars to which the date represented by the content of this element belongs, but this <name/> element has no textual content.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-abstractModel-structure-p-in-ab-or-p-12">
      <rule context="tei:p">
         <report test="(ancestor::tei:ab or ancestor::tei:p) and                        not( ancestor::tei:floatingText                           | parent::tei:exemplum                           | parent::tei:item                           | parent::tei:note                           | parent::tei:q                           | parent::tei:quote                           | parent::tei:remarks                           | parent::tei:said                           | parent::tei:sp                           | parent::tei:stage                           | parent::tei:cell                           | parent::tei:figure )"> Abstract model violation: Paragraphs may not occur inside other paragraphs or ab elements.</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-abstractModel-structure-p-in-l-13">
      <rule context="tei:l//tei:p">
         <assert test="ancestor::tei:floatingText | parent::tei:figure | parent::tei:note"> Abstract model violation: Metrical lines may not contain higher-level structural elements such as div, p, or ab, unless p is a child of figure or note, or is a descendant of floatingText.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-deprecationInfo-only-in-deprecated-14">
      <rule context="tei:desc[ @type eq 'deprecationInfo']">
         <assert test="../@validUntil">Information about a deprecation should only be present in a specification element that is being deprecated: that is, only an element that has a @validUntil attribute should have a child &lt;desc type="deprecationInfo"&gt;.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-rt-target-not-span-15">
      <rule context="tei:rt/@target">
         <report test="../@from | ../@to">When target= is present, neither from= nor to= should be.</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-rt-from-16">
      <rule context="tei:rt/@from">
         <assert test="../@to">When from= is present, the to= attribute of <name/> is required.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-rt-to-17">
      <rule context="tei:rt/@to">
         <assert test="../@from">When to= is present, the from= attribute of <name/> is required.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-ptrAtts-18">
      <rule context="tei:ptr">
         <report test="@target and @cRef">Only one of the attributes @target and @cRef may be supplied on <name/>.</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-refAtts-19">
      <rule context="tei:ref">
         <report test="@target and @cRef">Only one of the attributes @target and @cRef may be supplied on <name/>.</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-gloss-list-must-have-labels-20">
      <rule context="tei:list[@type='gloss']">
         <assert test="tei:label">The content of a "gloss" list should include a sequence of one or more pairs of a label element followed by an item element</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-targetorcontent1-21">
      <rule context="tei:relatedItem">
         <report test="@target and count( child::* ) &gt; 0">If the @target attribute on <name/> is used, the relatedItem element must be empty</report>
         <assert test="@target or child::*">A relatedItem element should have either a @target attribute or a child element to indicate the related bibliographic item</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-abstractModel-structure-l-in-l-22">
      <rule context="tei:l">
         <report test="ancestor::tei:l[not(.//tei:note//tei:l[. = current()])]">Abstract model violation: Lines may not contain lines or lg elements.</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-atleast1oflggapl-23">
      <rule context="tei:lg">
         <assert test="count(descendant::tei:lg|descendant::tei:l|descendant::tei:gap) &gt; 0">An lg element must contain at least one child l, lg, or gap element.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-abstractModel-structure-lg-in-l-24">
      <rule context="tei:lg">
         <report test="ancestor::tei:l[not(.//tei:note//tei:lg[. = current()])]">Abstract model violation: Lines may not contain line groups.</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-network_sp_with_who_attr-25">
      <rule context="tei:sp" role="warning">
         <assert test="@who"> A speech 'sp' without an attribute '@who' is not used when extracting the network. SHOULD consider linking the speech act to a speaking character ('person') in the 'particDesc'.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-network_unlinked_sp-26">
      <rule context="tei:sp[@who[not(contains(.,' '))]]" role="warning">
         <let name="localID" value="replace(@who/string(),'#','')"/>
         <assert test="ancestor::tei:TEI//tei:particDesc//(tei:person|tei:personGrp)[@xml:id eq $localID]"
                 role="warning"> A speech act SHOULD link to a 'person' or 'personGrp' element in 'particDesc'. Use a valid character ID and provide it as a pointer by prepending it with a hash '#'."</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-network_unlinked_sp-27">
      <rule context="tei:sp[contains(@who,' ')]">
         <let name="allIDs"
              value="./ancestor::tei:TEI//tei:particDesc//(tei:person|tei:personGrp)/@xml:id/string()"/>
         <let name="localIDs" value="tokenize(@who/string(),'\s+')"/>
         <assert test="every $i in $localIDs satisfies replace($i,'#','') = $allIDs"
                 role="warning"> At least one character ID provided as the value of the attribute who '<value-of xmlns="http://www.w3.org/1999/XSL/Transform" select="@who/string()"/>' has not been declared. A speech act SHOULD link to a 'person' or 'personGrp' element in 'particDesc'.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-play_main_title-28">
      <rule context="tei:titleStmt" role="warning">
         <assert test="tei:title[@type eq 'main']"> For the DraCor API to include the title of the play in the response an element 'title' with the type-attribute value 'main' SHOULD be included.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-identifier_corpus_name_in_corpus_xml-29">
      <rule context="tei:publicationStmt[ancestor::tei:teiCorpus]" role="critical">
         <assert test="tei:idno[@type eq 'URI']"> The identifier 'corpus name' of the corpus MUST be included as an element idno with the value 'URI' of the attribute 'type'.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-corpus_repository_url_in_corpus_xml-30">
      <rule context="tei:publicationStmt[ancestor::tei:teiCorpus]" role="warning">
         <assert test="tei:idno[@type eq 'repo']"> Consider specifying the GitHub repository of the corpus using an idno element of the type 'repo'.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-xml_base_required_on_idno_type_URI_in_corpus_xml-31">
      <rule context="tei:idno[@type eq 'URI' and parent::tei:publicationStmt and ancestor::tei:teiCorpus]"
            role="warning">
         <assert test="@xml:base/string() eq 'https://dracor.org/'"> The idno element in publicationStmt in the teiCorpus SHOULD have an xml:base attribute with the value 'https://dracor.org/'.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-digital_source_in_sourceDesc-32">
      <rule context="tei:sourceDesc">
         <assert test="tei:bibl[@type eq 'digitalSource']"> Digital source is missing</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-original_source_in_sourceDesc-33">
      <rule context="tei:sourceDesc/tei:bibl[@type eq 'digitalSource']">
         <assert test="tei:bibl[@type eq 'originalSource']"> Original Source for digital source is missing</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-quotationContents-34">
      <rule context="tei:quotation">
         <report test="not( @marks )  and  not( tei:p )"> On <name/>, either the @marks attribute should be used, or a paragraph of description provided</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-citestructure-outer-match-35">
      <rule context="tei:citeStructure[not(parent::tei:citeStructure)]">
         <assert test="starts-with(@match,'/')">An XPath in @match on the outer <name/> must start with '/'.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-citestructure-inner-match-36">
      <rule context="tei:citeStructure[parent::tei:citeStructure]">
         <assert test="not(starts-with(@match,'/'))">An XPath in @match must not start with '/' except on the outer <name/>.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-valid_dracor_ids_on_root_tei_element-37">
      <rule context="tei:TEI" role="warning">
         <assert test="matches(./@xml:id,'^[a-z]+[0-9]{6}$')"> For DraCor IDs we recommend the pattern ^[a-z]+[0-9]{6}$</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-network_check_basic_play_structure_div-38">
      <rule context="tei:body" role="warning">
         <assert test="tei:div"> A play SHOULD at least have one structural division 'div' for the API to be able to extract a network.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-network_check_basic_play_structure_sp-39">
      <rule context="tei:body" role="warning">
         <assert test=".//tei:sp"> A play SHOULD be structured in speech-acts using the element 'sp' for the API to be able to extract a network.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-network_play_without_speaking_characters-40">
      <rule context="tei:body[not(.//tei:sp)]" role="warning">
         <assert test=".//tei:stage" role="warning"> A drama that does not contain a speech-act 'sp', SHOULD at least contain a stage direction 'stage'.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-abstractModel-structure-div-in-l-41">
      <rule context="tei:l//tei:div">
         <assert test="ancestor::tei:floatingText"> Abstract model violation: Metrical lines may not contain higher-level structural elements such as div, unless div is a descendant of floatingText.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-abstractModel-structure-div-in-ab-or-p-42">
      <rule context="tei:div">
         <report test="(ancestor::tei:p or ancestor::tei:ab) and not(ancestor::tei:floatingText)"> Abstract model violation: p and ab may not contain higher-level structural elements such as div, unless div is a descendant of floatingText.</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-linkTargets3-43">
      <rule context="tei:link">
         <assert test="contains(normalize-space(@target),' ')">You must supply at least two values for @target or on <name/>
         </assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-encoding-hint_ab_used_somewhere_else-44">
      <rule context="tei:ab"
            role="warning"
            see="https://dracor.org/doc/odd#TEI.ab">
         <assert test="./parent::tei:licence"> In DraCor the element 'ab' should only be used to mark the label of the licence in the teiHeader.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-abstractModel-structure-ab-in-l-45">
      <rule context="tei:l//tei:ab">
         <assert test="ancestor::tei:floatingText | parent::tei:figure | parent::tei:note"> Abstract model violation: Metrical lines may not contain higher-level divisions such as p or ab, unless ab is a child of figure or note, or is a descendant of floatingText.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-joinTargets3-46">
      <rule context="tei:join">
         <assert test="contains( normalize-space( @target ),' ')"> You must supply at least two values for @target on <name/>
         </assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-nested_standOff_should_be_typed-47">
      <rule context="tei:standOff">
         <assert test="@type or not(ancestor::tei:standOff)">This <name/> element must have a @type attribute, since it is nested inside a <name/>
         </assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-person_sex-48">
      <rule context="tei:person[@sex]"
            role="warning"
            see="https://dracor.org/doc/odd#section-character-sex-gender">
         <assert test="@sex = ('FEMALE', 'MALE', 'UNKNOWN')"> The values for person/@sex supported by the DraCor API are <val xmlns="http://www.tei-c.org/ns/1.0"
                 xmlns:crm="http://www.cidoc-crm.org/cidoc-crm/"
                 xmlns:dig="http://www.ics.forth.gr/isl/CRMdig/"
                 xmlns:owl="http://www.w3.org/2002/07/owl#"
                 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
                 xmlns:rng="http://relaxng.org/ns/structure/1.0"
                 xmlns:sch="http://purl.oclc.org/dsdl/schematron"
                 xmlns:sqf="http://www.schematron-quickfix.com/validator/process"
                 xmlns:tei="http://www.tei-c.org/ns/1.0"
                 xmlns:teix="http://www.tei-c.org/ns/Examples"
                 xmlns:xi="http://www.w3.org/2001/XInclude"
                 xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
                 xmlns:xsl="http://www.w3.org/1999/XSL/Transform">FEMALE</val>, <val xmlns="http://www.tei-c.org/ns/1.0"
                 xmlns:crm="http://www.cidoc-crm.org/cidoc-crm/"
                 xmlns:dig="http://www.ics.forth.gr/isl/CRMdig/"
                 xmlns:owl="http://www.w3.org/2002/07/owl#"
                 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
                 xmlns:rng="http://relaxng.org/ns/structure/1.0"
                 xmlns:sch="http://purl.oclc.org/dsdl/schematron"
                 xmlns:sqf="http://www.schematron-quickfix.com/validator/process"
                 xmlns:tei="http://www.tei-c.org/ns/1.0"
                 xmlns:teix="http://www.tei-c.org/ns/Examples"
                 xmlns:xi="http://www.w3.org/2001/XInclude"
                 xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
                 xmlns:xsl="http://www.w3.org/1999/XSL/Transform">MALE</val> and <val xmlns="http://www.tei-c.org/ns/1.0"
                 xmlns:crm="http://www.cidoc-crm.org/cidoc-crm/"
                 xmlns:dig="http://www.ics.forth.gr/isl/CRMdig/"
                 xmlns:owl="http://www.w3.org/2002/07/owl#"
                 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
                 xmlns:rng="http://relaxng.org/ns/structure/1.0"
                 xmlns:sch="http://purl.oclc.org/dsdl/schematron"
                 xmlns:sqf="http://www.schematron-quickfix.com/validator/process"
                 xmlns:tei="http://www.tei-c.org/ns/1.0"
                 xmlns:teix="http://www.tei-c.org/ns/Examples"
                 xmlns:xi="http://www.w3.org/2001/XInclude"
                 xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
                 xmlns:xsl="http://www.w3.org/1999/XSL/Transform">UNKNOWN</val>.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-personGrp_sex-49">
      <rule context="tei:personGrp[@sex]"
            role="warning"
            see="https://dracor.org/doc/odd#section-character-sex-gender">
         <assert test="every $t in tokenize(@sex) satisfies $t = ('FEMALE', 'MALE', 'UNKNOWN')"> The values for personGrp/@sex supported by the DraCor API are <val xmlns="http://www.tei-c.org/ns/1.0"
                 xmlns:crm="http://www.cidoc-crm.org/cidoc-crm/"
                 xmlns:dig="http://www.ics.forth.gr/isl/CRMdig/"
                 xmlns:owl="http://www.w3.org/2002/07/owl#"
                 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
                 xmlns:rng="http://relaxng.org/ns/structure/1.0"
                 xmlns:sch="http://purl.oclc.org/dsdl/schematron"
                 xmlns:sqf="http://www.schematron-quickfix.com/validator/process"
                 xmlns:tei="http://www.tei-c.org/ns/1.0"
                 xmlns:teix="http://www.tei-c.org/ns/Examples"
                 xmlns:xi="http://www.w3.org/2001/XInclude"
                 xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
                 xmlns:xsl="http://www.w3.org/1999/XSL/Transform">FEMALE</val>, <val xmlns="http://www.tei-c.org/ns/1.0"
                 xmlns:crm="http://www.cidoc-crm.org/cidoc-crm/"
                 xmlns:dig="http://www.ics.forth.gr/isl/CRMdig/"
                 xmlns:owl="http://www.w3.org/2002/07/owl#"
                 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
                 xmlns:rng="http://relaxng.org/ns/structure/1.0"
                 xmlns:sch="http://purl.oclc.org/dsdl/schematron"
                 xmlns:sqf="http://www.schematron-quickfix.com/validator/process"
                 xmlns:tei="http://www.tei-c.org/ns/1.0"
                 xmlns:teix="http://www.tei-c.org/ns/Examples"
                 xmlns:xi="http://www.w3.org/2001/XInclude"
                 xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
                 xmlns:xsl="http://www.w3.org/1999/XSL/Transform">MALE</val> and <val xmlns="http://www.tei-c.org/ns/1.0"
                 xmlns:crm="http://www.cidoc-crm.org/cidoc-crm/"
                 xmlns:dig="http://www.ics.forth.gr/isl/CRMdig/"
                 xmlns:owl="http://www.w3.org/2002/07/owl#"
                 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
                 xmlns:rng="http://relaxng.org/ns/structure/1.0"
                 xmlns:sch="http://purl.oclc.org/dsdl/schematron"
                 xmlns:sqf="http://www.schematron-quickfix.com/validator/process"
                 xmlns:tei="http://www.tei-c.org/ns/1.0"
                 xmlns:teix="http://www.tei-c.org/ns/Examples"
                 xmlns:xi="http://www.w3.org/2001/XInclude"
                 xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
                 xmlns:xsl="http://www.w3.org/1999/XSL/Transform">UNKNOWN</val>.</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-ref-or-key-or-name-50">
      <rule context="tei:relation">
         <assert test="@ref or @key or @name">One of the attributes @name, @ref or @key must be supplied</assert>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-active-mutual-51">
      <rule context="tei:relation">
         <report test="@active and @mutual">Only one of the attributes @active and @mutual may be supplied</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-active-passive-52">
      <rule context="tei:relation">
         <report test="@passive and not(@active)">the attribute @passive may be supplied only if the attribute @active is supplied</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-noNestedS-53">
      <rule context="tei:s">
         <report test="tei:s">You may not nest one s element within another: use seg instead</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-target-from-54">
      <rule context="tei:span">
         <report test="@from and @target"> Only one of the attributes @target and @from may be supplied on <name/>
         </report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-targetto-55">
      <rule context="tei:span">
         <report test="@to and @target"> Only one of the attributes @target and @to may be supplied on <name/>
         </report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-tonotfrom-56">
      <rule context="tei:span">
         <report test="@to and not(@from)"> If @to is supplied on <name/>, @from must be supplied as well</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-tofrom-57">
      <rule context="tei:span">
         <report test="contains(normalize-space(@to),' ') or contains(normalize-space(@from),' ')"> The attributes @to and @from on <name/> may each contain only a single value</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-play_id-58">
      <rule context="/"
            role="information"
            see="https://dracor.org/doc/odd#play_id">
         <let name="play_id" value="/tei:TEI/@xml:id/string()"/>
         <report test="/tei:TEI/@xml:id"> Supported API feature: play_id [value: <value-of select="$play_id"/>]</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-play_wikidata_id-59">
      <rule context="/"
            role="information"
            see="https://dracor.org/doc/odd#play_wikidata_id">
         <let name="play_wikidata"
              value="/tei:TEI/tei:standOff/tei:listRelation/tei:relation[@name eq 'wikidata']/@passive/string()"/>
         <report test="/tei:TEI/tei:standOff/tei:listRelation/tei:relation[@name eq 'wikidata']/@passive[starts-with(.,'http://www.wikidata.org/entity/')]"> Supported API feature: play_wikidata_id [value: <value-of select="substring-after($play_wikidata, 'http://www.wikidata.org/entity/')"/>]</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-play_title-60">
      <rule context="/"
            role="information"
            see="https://dracor.org/doc/odd#play_title">
         <let name="play_title"
              value="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[not(@type = 'sub') and not(@xml:lang or ./ancestor::tei:TEI/@xml:lang = @xml:lang)]/normalize-space()"/>
         <report test="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[not(@type = 'sub') and not(@xml:lang or ./ancestor::tei:TEI/@xml:lang = @xml:lang)]"> Supported API feature: play_title [value: <value-of select="$play_title"/>]</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-play_subtitle-61">
      <rule context="/"
            role="information"
            see="https://dracor.org/doc/odd#play_subtitle">
         <let name="play_subtitle"
              value="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type = 'sub' and not(@xml:lang or ./ancestor::tei:TEI/@xml:lang = @xml:lang)]/normalize-space()"/>
         <report test="tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type='sub']"> Supported API feature: play_subtitle [value: <value-of select="$play_subtitle"/>]</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-play_year_written-62">
      <rule context="/"
            role="information"
            see="https://dracor.org/doc/odd#play_year_written">
         <let name="play_year_written"
              value="/tei:TEI/tei:standOff/tei:listEvent/tei:event[@type eq 'written']/@when/string()"/>
         <report test="/tei:TEI/tei:standOff/tei:listEvent/tei:event[@type eq 'written']/@when"> Supported API feature: play_year_written [value: <value-of select="$play_year_written"/>]</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-play_year_printed-63">
      <rule context="/"
            role="information"
            see="https://dracor.org/doc/odd#play_year_printed">
         <let name="play_year_printed"
              value="/tei:TEI/tei:standOff/tei:listEvent/tei:event[@type eq 'print']/@when/string()"/>
         <report test="/tei:TEI/tei:standOff/tei:listEvent/tei:event[@type eq 'print']/@when"> Supported API feature: play_year_printed [value: <value-of select="$play_year_printed"/>]</report>
      </rule>
   </pattern>
   <pattern id="schematron-constraint-encoding-hint_play_wikidata_id-64">
      <rule context="tei:TEI/tei:standOff/tei:listRelation/tei:relation[@name eq 'wikidata']"
            role="warning"
            see="https://dracor.org/doc/odd#section-play-wikidata">
         <let name="expected_play_uri"
              value="concat('https://dracor.org/entity/',./ancestor::tei:TEI/@xml:id/string())"/>
         <assert test="starts-with(./@passive/string(),'http://www.wikidata.org/entity/')"> The value of the attribute '@passive' MUST start with "http://www.wikidata.org/entity/" to be a valid Wikidata URI.</assert>
         <assert test="./@active/string() eq $expected_play_uri"> The value of the attribute '@active' SHOULD follow the pattern "https://dracor.org/entity/{play_id}. [Expected value: '<value-of xmlns="http://www.w3.org/1999/XSL/Transform" select="$expected_play_uri"/>']"</assert>
      </rule>
   </pattern>
</schema>
