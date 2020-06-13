


cdl_att = read.dbf("I:/GIS-Raster/CDL/generic_cdl_attributes.tif.vat.dbf/ESRI_attribute_files/ArcGIS10.7.0_2019_30m_cdls.img.vat.dbf") %>%
  dplyr::select(value=VALUE, name=CLASS_NAME)
write_csv(cdl_att, "I:/GIS-Raster/CDL/cdl-grouping-table.csv")

cdl_att = read_csv("I:/GIS-Raster/CDL/cdl-grouping-table.csv")

stateclass = raster("data/initial-conditions/ic-state-class.tif")
stateclass_ag = reclassify(stateclass, c(0,80,NA, 80.5,89.5,1, 89.6,Inf,NA))

cdl2010 = raster("I:/GIS-Raster/CDL/2010_30m_cdls/2010_30m_cdls.img")
cdl2010 = projectRaster(cdl2010, stateclass, method="ngb")
cdl2010 = mask(cdl2010, stateclass)

cdl2016 = raster("I:/GIS-Raster/CDL/2016_30m_cdls/2016_30m_cdls.img")
cdl2016 = projectRaster(cdl2016, stateclass, method="ngb")
cdl2016 = mask(cdl2016, stateclass)

v = tibble(
  cdl10 = values(cdl2010),
  cdl16 = values(cdl2016))

# Create contigency table of amounts and probabilities and dataframe (using amounts; m variables)
m1 = table(v[,c("cdl10", "cdl16")])
p1 = as.matrix(m1 / rowSums(m1))
d1 = m1 %>% as_tibble() %>% dplyr::select(from=cdl10, to=cdl16, p1=n) %>%
  mutate(from = as.numeric(from), to = as.numeric(to)) %>%
  left_join(cdl_att, by = c("from" = "value")) %>% rename("fromclass"="name", "fromgroup"="group") %>%
  left_join(cdl_att, by = c("to" = "value")) %>% rename("toclass"="name", "togroup"="group") %>%
  filter(!is.na(togroup), !is.na(fromgroup)) %>%
  group_by(fromgroup, togroup) %>%
  summarise(area = sum(p1)) %>%
  pivot_wider(names_from = togroup, values_from = area)




cdl_mask = mask(cdl2016, stateclass_ag)
plot(cdl_mask)
cdl2016 = as_tibble(freq(cdl_mask)) %>%
  left_join(cdl_att) %>%
  filter(!is.na(name)) %>%
  filter(value<100 | value >200)

ggplot(cdl2016, aes(x=fct_reorder(group, count), y=count, fill=name)) +
  geom_bar(stat = "identity") +
  coord_flip()


unique(cdl_att$name)
