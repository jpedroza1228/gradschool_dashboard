# scripts for dashboard


### Correlations between GE Teaching FTE & Undergraduate 

```{r}
wide_lm_list <- wide %>% 
  drop_na(college, course_dept, fte, ge_teach_fte) %>% 
  group_by(college, course_dept) %>% 
  nest() %>% 
  mutate(lm_model = map(data, ~lm(fte ~ ge_teach_fte, data = .x)),
         lm_coefs = map(lm_model, coef),
         ge_teach_fte_b = map_dbl(lm_coefs, 2),
         lm_r2 = map_dbl(lm_model, ~summary(.x)$r.squared),
         lm_pvalue = map_dbl(lm_model, ~summary(.x)$coefficients[, 4]['ge_teach_fte']),
         lm_r = map_dbl(lm_r2, ~sqrt(.x)))

wide_lm_list %>% 
  dplyr::select(college, course_dept, lm_r, lm_pvalue) %>% 
  rename(`College` = college,
         `Course Department` = course_dept) %>% 
  mutate(r = round(lm_r, digits = 3),
         `p value` = round(lm_pvalue, digits = 3)) %>% 
  dplyr::select(-lm_r, -lm_pvalue) %>% 
  reactable::reactable()
```



College of Arts & Sciences
=======================================================================
  
  Column {data-width=650}
-----------------------------------------------------------------------
  
  ### Chart A
  
  ```{r}
cas_mean_plot <- wide %>% 
  filter(ge_teach == 1 &
           college == 'College of Arts & Sciences') %>% 
  drop_na(ge_teach, ge_teach_fte, fte) %>% 
  group_by(course_dept, ge_teach) %>%
  mutate(mean_fte = mean(ge_teach_fte),
         ge_teach_n = n(),
         mean_dept_fte = mean(fte)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(course_dept, mean_fte, size = ge_teach_n, color = course_dept)) +
  geom_point(aes(course_dept, mean_dept_fte, size = ge_teach_n, color = course_dept)) +
  coord_flip() +
  scale_y_continuous(breaks = c(.1, .2, .3, .4, .5),
                     limits = c(.1, .5)) +
  labs(x = ' ',
       y = 'Mean GE and Course FTE') +
  theme(legend.position = 'none') +
  scale_color_manual(values = c('#5e2228', '#d22a2c', '#9b2a30', '#c80010',
                                '#6a1f25', '#d72d2b', '#5b2c2f', '#387448',
                                '#c0c5ba', '#888980', '#344039', '#4f4c3c',
                                '#807f67', '#386350', '#387448', '#5b6242',
                                '#669b3e', '#808572', '#7fb7b7', '#53636b',
                                '#85b3c1', '#507b8b', '#2c6195', '#4b5f71',
                                '#4b5f71', '#3f6698', '#26657f', '#398b98',
                                '#ddca72', '#c3b37d', '#d3bb95', '#c4bb9b',
                                '#a26540',
                                '#dbd5a5', '#a86428', '#c7a860', '#c0cc56', 
                                '#e5e1d4', '#d3bb95', '#a26540', '#ead568',
                                '#7f5d47', '#b23a2d', '#c33431', 'd74122',
                                '#b23a2d'))

ggplotly(cas_mean_plot)
```

Column {data-width=350}
-----------------------------------------------------------------------
  
  ### Chart B
  
  ```{r}
cas_scat <- wide %>% 
  filter(college == 'College of Arts & Sciences' &
           ge_teach_fte < .5 &
           ge_teach != 0) %>% 
  drop_na(ge_teach_fte, fte) %>% 
  ggplot(aes(ge_teach_fte, fte)) +
  geom_point(color = 'gray70', alpha = .7) +
  geom_smooth(aes(color = as.factor(course_dept)), method = 'lm', se = FALSE) +
  theme(legend.position = 'none') 
# scale_color_manual(values = c())

ggplotly(cas_scat) %>% 
  partial_bundle()
```

### Chart C

```{r}

```

College of Design
=======================================================================
  
  Column {data-width=650}
-----------------------------------------------------------------------
  
  ### Chart A
  
  ```{r}
cod_mean_plot <- wide %>% 
  filter(ge_teach == 1 &
           college == 'College of Design') %>% 
  drop_na(ge_teach, ge_teach_fte, fte) %>% 
  group_by(course_dept, ge_teach) %>%
  mutate(mean_fte = mean(ge_teach_fte),
         ge_teach_n = n(),
         mean_dept_fte = mean(fte)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(course_dept, mean_fte, size = ge_teach_n, color = course_dept)) +
  geom_point(aes(course_dept, mean_dept_fte, size = ge_teach_n, color = course_dept)) +
  coord_flip() +
  scale_y_continuous(breaks = c(.1, .2, .3, .4, .5),
                     limits = c(.1, .5)) +
  labs(x = ' ',
       y = 'Mean GE and Course FTE') +
  theme(legend.position = 'none') +
  scale_color_manual(values = c('#5e2228', '#d22a2c', '#9b2a30', '#c80010',
                                '#6a1f25', '#d72d2b', '#5b2c2f', '#387448',
                                '#c0c5ba', '#888980', '#344039', '#4f4c3c',
                                '#807f67', '#386350', '#387448', '#5b6242',
                                '#669b3e', '#808572', '#7fb7b7', '#53636b',
                                '#85b3c1', '#507b8b', '#2c6195', '#4b5f71',
                                '#4b5f71', '#3f6698', '#26657f', '#398b98',
                                '#ddca72', '#c3b37d', '#d3bb95', '#c4bb9b',
                                '#a26540',
                                '#dbd5a5', '#a86428', '#c7a860', '#c0cc56', 
                                '#e5e1d4', '#d3bb95', '#a26540', '#ead568',
                                '#7f5d47', '#b23a2d', '#c33431', 'd74122',
                                '#b23a2d'))

ggplotly(cod_mean_plot)
```

Column {data-width=350}
-----------------------------------------------------------------------
  
  ### Chart B
  
  ```{r}
cod_scat <- wide %>% 
  filter(college == 'College of Design' &
           ge_teach_fte < .5 &
           ge_teach != 0) %>% 
  drop_na(ge_teach_fte, fte) %>% 
  ggplot(aes(ge_teach_fte, fte)) +
  geom_point(color = 'gray70', alpha = .7) +
  geom_smooth(aes(color = as.factor(course_dept)), method = 'lm', se = FALSE) +
  theme(legend.position = 'none') 
# scale_color_manual(values = c())

ggplotly(cod_scat) %>% 
  partial_bundle()
```

### Chart C

```{r}

```

College of Education
=======================================================================
  
  Column {data-width=650}
-----------------------------------------------------------------------
  
  ### Chart A
  
  ```{r}
coe_mean_plot <- wide %>% 
  filter(ge_teach == 1 &
           college == 'College of Education') %>% 
  drop_na(ge_teach, ge_teach_fte, fte) %>% 
  group_by(course_dept, ge_teach) %>%
  mutate(mean_fte = mean(ge_teach_fte),
         ge_teach_n = n(),
         mean_dept_fte = mean(fte)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(course_dept, mean_fte, size = ge_teach_n, color = course_dept)) +
  geom_point(aes(course_dept, mean_dept_fte, size = ge_teach_n, color = course_dept)) +
  coord_flip() +
  scale_y_continuous(breaks = c(.1, .2, .3, .4, .5),
                     limits = c(.1, .5)) +
  labs(x = ' ',
       y = 'Mean GE and Course FTE') +
  theme(legend.position = 'none') +
  scale_color_manual(values = c('#5e2228', '#d22a2c', '#9b2a30', '#c80010',
                                '#6a1f25', '#d72d2b', '#5b2c2f', '#387448',
                                '#c0c5ba', '#888980', '#344039', '#4f4c3c',
                                '#807f67', '#386350', '#387448', '#5b6242',
                                '#669b3e', '#808572', '#7fb7b7', '#53636b',
                                '#85b3c1', '#507b8b', '#2c6195', '#4b5f71',
                                '#4b5f71', '#3f6698', '#26657f', '#398b98',
                                '#ddca72', '#c3b37d', '#d3bb95', '#c4bb9b',
                                '#a26540',
                                '#dbd5a5', '#a86428', '#c7a860', '#c0cc56', 
                                '#e5e1d4', '#d3bb95', '#a26540', '#ead568',
                                '#7f5d47', '#b23a2d', '#c33431', 'd74122',
                                '#b23a2d'))

ggplotly(coe_mean_plot)
```

Column {data-width=350}
-----------------------------------------------------------------------
  
  ### Chart B
  
  ```{r}
coe_scat <- wide %>% 
  filter(college == 'College of Education' &
           ge_teach_fte < .5 &
           ge_teach != 0) %>% 
  drop_na(ge_teach_fte, fte) %>% 
  ggplot(aes(ge_teach_fte, fte)) +
  geom_point(aes(color = as.factor(course_dept)), alpha = .5) +
  geom_smooth(aes(color = as.factor(course_dept)), method = 'lm', se = FALSE) +
  theme(legend.position = 'none') 
# scale_color_manual(values = c())

ggplotly(coe_scat) %>% 
  partial_bundle()
```

### Chart C

```{r}

```

Lundquist College of Business
=======================================================================
  
  Column {data-width=650}
-----------------------------------------------------------------------
  
  ### Chart A
  
  ```{r}
lcob_mean_plot <- wide %>% 
  filter(ge_teach == 1 &
           college == 'Lundquist College of Business') %>% 
  drop_na(ge_teach, ge_teach_fte, fte) %>% 
  group_by(course_dept, ge_teach) %>%
  mutate(mean_fte = mean(ge_teach_fte),
         ge_teach_n = n(),
         mean_dept_fte = mean(fte)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(course_dept, mean_fte, size = ge_teach_n, color = course_dept)) +
  geom_point(aes(course_dept, mean_dept_fte, size = ge_teach_n, color = course_dept)) +
  coord_flip() +
  scale_y_continuous(breaks = c(.1, .2, .3, .4, .5),
                     limits = c(.1, .5)) +
  labs(x = ' ',
       y = 'Mean GE and Course FTE') +
  theme(legend.position = 'none') +
  scale_color_manual(values = c('#5e2228', '#d22a2c', '#9b2a30', '#c80010',
                                '#6a1f25', '#d72d2b', '#5b2c2f', '#387448',
                                '#c0c5ba', '#888980', '#344039', '#4f4c3c',
                                '#807f67', '#386350', '#387448', '#5b6242',
                                '#669b3e', '#808572', '#7fb7b7', '#53636b',
                                '#85b3c1', '#507b8b', '#2c6195', '#4b5f71',
                                '#4b5f71', '#3f6698', '#26657f', '#398b98',
                                '#ddca72', '#c3b37d', '#d3bb95', '#c4bb9b',
                                '#a26540',
                                '#dbd5a5', '#a86428', '#c7a860', '#c0cc56', 
                                '#e5e1d4', '#d3bb95', '#a26540', '#ead568',
                                '#7f5d47', '#b23a2d', '#c33431', 'd74122',
                                '#b23a2d'))

ggplotly(lcob_mean_plot)
```

Column {data-width=350}
-----------------------------------------------------------------------
  
  ### Chart B
  
  ```{r}
lcob_scat <- wide %>% 
  filter(college == 'Lundquist College of Business' &
           ge_teach_fte < .5 &
           ge_teach != 0) %>% 
  drop_na(ge_teach_fte, fte) %>% 
  ggplot(aes(ge_teach_fte, fte)) +
  geom_point(aes(color = as.factor(course_dept)), alpha = .5) +
  geom_smooth(aes(color = as.factor(course_dept)), method = 'lm', se = FALSE) +
  theme(legend.position = 'none') 
# scale_color_manual(values = c())

ggplotly(lcob_scat) %>% 
  partial_bundle()
```

### Chart C

```{r}

```

School of Journalism & Communication
=======================================================================
  
  Column {data-width=650}
-----------------------------------------------------------------------
  
  ### Chart A
  
  ```{r}
sojc_mean_plot <- wide %>% 
  filter(ge_teach == 1 &
           college == 'School of Journalism & Communication') %>% 
  drop_na(ge_teach, ge_teach_fte, fte) %>% 
  group_by(course_dept, ge_teach) %>%
  mutate(mean_fte = mean(ge_teach_fte),
         ge_teach_n = n(),
         mean_dept_fte = mean(fte)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(course_dept, mean_fte, size = ge_teach_n, color = course_dept)) +
  geom_point(aes(course_dept, mean_dept_fte, size = ge_teach_n, color = course_dept)) +
  coord_flip() +
  scale_y_continuous(breaks = c(.1, .2, .3, .4, .5),
                     limits = c(.1, .5)) +
  labs(x = ' ',
       y = 'Mean GE and Course FTE') +
  theme(legend.position = 'none') +
  scale_color_manual(values = c('#5e2228', '#d22a2c', '#9b2a30', '#c80010',
                                '#6a1f25', '#d72d2b', '#5b2c2f', '#387448',
                                '#c0c5ba', '#888980', '#344039', '#4f4c3c',
                                '#807f67', '#386350', '#387448', '#5b6242',
                                '#669b3e', '#808572', '#7fb7b7', '#53636b',
                                '#85b3c1', '#507b8b', '#2c6195', '#4b5f71',
                                '#4b5f71', '#3f6698', '#26657f', '#398b98',
                                '#ddca72', '#c3b37d', '#d3bb95', '#c4bb9b',
                                '#a26540',
                                '#dbd5a5', '#a86428', '#c7a860', '#c0cc56', 
                                '#e5e1d4', '#d3bb95', '#a26540', '#ead568',
                                '#7f5d47', '#b23a2d', '#c33431', 'd74122',
                                '#b23a2d'))

ggplotly(sojc_mean_plot)
```

Column {data-width=350}
-----------------------------------------------------------------------
  
  ### Chart B
  
  ```{r}
sojc_scat <- wide %>% 
  filter(college == 'School of Journalism & Communication' &
           ge_teach_fte < .5 &
           ge_teach != 0) %>% 
  drop_na(ge_teach_fte, fte) %>% 
  ggplot(aes(ge_teach_fte, fte)) +
  geom_point(aes(color = as.factor(course_dept)), alpha = .5) +
  geom_smooth(aes(color = as.factor(course_dept)), method = 'lm', se = FALSE) +
  theme(legend.position = 'none') 
# scale_color_manual(values = c())

ggplotly(sojc_scat) %>% 
  partial_bundle()
```

### Chart C

```{r}

```

School of Law
=======================================================================
  
  Column {data-width=650}
-----------------------------------------------------------------------
  
  ### Chart A
  
  ```{r}
law_mean_plot <- wide %>% 
  filter(ge_teach == 1 &
           college == 'School of Law') %>% 
  drop_na(ge_teach, ge_teach_fte, fte) %>% 
  group_by(course_dept, ge_teach) %>%
  mutate(mean_fte = mean(ge_teach_fte),
         ge_teach_n = n(),
         mean_dept_fte = mean(fte)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(course_dept, mean_fte, size = ge_teach_n, color = course_dept)) +
  geom_point(aes(course_dept, mean_dept_fte, size = ge_teach_n, color = course_dept)) +
  coord_flip() +
  scale_y_continuous(breaks = c(.1, .2, .3, .4, .5),
                     limits = c(.1, .5)) +
  labs(x = ' ',
       y = 'Mean GE and Course FTE') +
  theme(legend.position = 'none') +
  scale_color_manual(values = c('#5e2228', '#d22a2c', '#9b2a30', '#c80010',
                                '#6a1f25', '#d72d2b', '#5b2c2f', '#387448',
                                '#c0c5ba', '#888980', '#344039', '#4f4c3c',
                                '#807f67', '#386350', '#387448', '#5b6242',
                                '#669b3e', '#808572', '#7fb7b7', '#53636b',
                                '#85b3c1', '#507b8b', '#2c6195', '#4b5f71',
                                '#4b5f71', '#3f6698', '#26657f', '#398b98',
                                '#ddca72', '#c3b37d', '#d3bb95', '#c4bb9b',
                                '#a26540',
                                '#dbd5a5', '#a86428', '#c7a860', '#c0cc56', 
                                '#e5e1d4', '#d3bb95', '#a26540', '#ead568',
                                '#7f5d47', '#b23a2d', '#c33431', 'd74122',
                                '#b23a2d'))

ggplotly(law_mean_plot)
```

Column {data-width=350}
-----------------------------------------------------------------------
  
  ### Chart B
  
  ```{r}
law_scat <- wide %>% 
  filter(college == 'School of Law' &
           ge_teach_fte < .5 &
           ge_teach != 0) %>% 
  drop_na(ge_teach_fte, fte) %>% 
  ggplot(aes(ge_teach_fte, fte)) +
  geom_point(aes(color = as.factor(course_dept)), alpha = .5) +
  geom_smooth(aes(color = as.factor(course_dept)), method = 'lm', se = FALSE) +
  theme(legend.position = 'none') 
# scale_color_manual(values = c())

ggplotly(law_scat) %>% 
  partial_bundle()
```

### Chart C

```{r}

```

School of Music & Dance
=======================================================================
  
  Column {data-width=650}
-----------------------------------------------------------------------
  
  ### Chart A
  
  ```{r}
somd_mean_plot <- wide %>% 
  filter(ge_teach == 1 &
           college == 'School of Music & Dance') %>% 
  drop_na(ge_teach, ge_teach_fte, fte) %>% 
  group_by(course_dept, ge_teach) %>%
  mutate(mean_fte = mean(ge_teach_fte),
         ge_teach_n = n(),
         mean_dept_fte = mean(fte)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(course_dept, mean_fte, size = ge_teach_n, color = course_dept)) +
  geom_point(aes(course_dept, mean_dept_fte, size = ge_teach_n, color = course_dept)) +
  coord_flip() +
  scale_y_continuous(breaks = c(.1, .2, .3, .4, .5),
                     limits = c(.1, .5)) +
  labs(x = ' ',
       y = 'Mean GE and Course FTE') +
  theme(legend.position = 'none') +
  scale_color_manual(values = c('#5e2228', '#d22a2c', '#9b2a30', '#c80010',
                                '#6a1f25', '#d72d2b', '#5b2c2f', '#387448',
                                '#c0c5ba', '#888980', '#344039', '#4f4c3c',
                                '#807f67', '#386350', '#387448', '#5b6242',
                                '#669b3e', '#808572', '#7fb7b7', '#53636b',
                                '#85b3c1', '#507b8b', '#2c6195', '#4b5f71',
                                '#4b5f71', '#3f6698', '#26657f', '#398b98',
                                '#ddca72', '#c3b37d', '#d3bb95', '#c4bb9b',
                                '#a26540',
                                '#dbd5a5', '#a86428', '#c7a860', '#c0cc56', 
                                '#e5e1d4', '#d3bb95', '#a26540', '#ead568',
                                '#7f5d47', '#b23a2d', '#c33431', 'd74122',
                                '#b23a2d'))

ggplotly(somd_mean_plot)
```

Column {data-width=350}
-----------------------------------------------------------------------
  
  ### Chart B
  
```{r}
somd_scat <- wide %>% 
  filter(college == 'School of Music & Dance' &
           ge_teach_fte < .5 &
           ge_teach != 0) %>% 
  drop_na(ge_teach_fte, fte) %>% 
  ggplot(aes(ge_teach_fte, fte)) +
  geom_point(aes(color = as.factor(course_dept)), alpha = .5) +
  geom_smooth(aes(color = as.factor(course_dept)), method = 'lm', se = FALSE) +
  theme(legend.position = 'none') 
# scale_color_manual(values = c())

ggplotly(somd_scat) %>% 
  partial_bundle()
```

### Chart C

```{r}

```











mean_fte_plot <- wide %>% 
  filter(ge_teach == 1 &
           college != 'Graduate School') %>% 
  group_by(course_dept, ge_teach) %>%
  mutate(mean_fte = mean(ge_teach_fte),
         ge_teach_n = n(),
         mean_dept_fte = mean(fte)) %>%
  ungroup() %>%
  ggplot(aes(college, mean_fte, size = ge_teach_n)) +
  geom_point(aes(color = course_dept)) +
  coord_flip() +
  scale_y_continuous(breaks = c(.2, .3, .4, .5),
                     limits = c(.2, .5)) +
  labs(x = 'Mean FTE',
       y = ' ') +
  theme(legend.position = 'none') +
  scale_color_manual(values = c('#5e2228', '#d22a2c', '#9b2a30', '#c80010',
                     '#6a1f25', '#d72d2b', '#5b2c2f', '#387448',
                     '#c0c5ba', '#888980', '#344039', '#4f4c3c',
                     '#807f67', '#386350', '#387448', '#5b6242',
                     '#669b3e', '#808572', '#7fb7b7', '#53636b',
                     '#85b3c1', '#507b8b', '#2c6195', '#4b5f71',
                     '#4b5f71', '#3f6698', '#26657f', '#398b98',
                     '#ddca72', '#c3b37d', '#d3bb95', '#c4bb9b',
                     '#a26540',
                     '#dbd5a5', '#a86428', '#c7a860', '#c0cc56', 
                     '#e5e1d4', '#d3bb95', '#a26540', '#ead568',
                     '#7f5d47', '#b23a2d', '#c33431', 'd74122',
                     '#b23a2d'))

ggplotly(mean_fte_plot)


# cas_scat <- 
  wide %>% 
  filter(college == 'College of Arts & Sciences' &
           ge_teach_fte < .5 &
           ge_teach != 0) %>% 
  drop_na(ge_teach_fte, fte) %>% 
  ggplot(aes(ge_teach_fte, fte)) +
  geom_point(color = 'gray70', alpha = .7) +
  geom_smooth(aes(color = as.factor(course_dept)), method = 'lm', se = FALSE) +
  theme(legend.position = 'none') +
  scale_color_manual(values = c('#5e2228', '#d22a2c', '#9b2a30', '#c80010',
                                '#6a1f25', '#d72d2b', '#5b2c2f', '#387448',
                                '#c0c5ba', '#888980', '#344039', '#4f4c3c',
                                '#807f67', '#386350', '#387448', '#5b6242',
                                '#669b3e', '#808572', '#7fb7b7', '#53636b',
                                '#85b3c1', '#507b8b', '#2c6195', '#4b5f71',
                                '#4b5f71', '#3f6698', '#26657f', '#398b98',
                                '#ddca72', '#c3b37d', '#d3bb95', '#c4bb9b',
                                '#a26540',
                                '#dbd5a5', '#a86428', '#c7a860', '#c0cc56', 
                                '#e5e1d4', '#d3bb95', '#a26540', '#ead568',
                                '#7f5d47', '#b23a2d', '#c33431', 'd74122',
                                '#b23a2d'))

ggplotly(cas_scat) %>% 
  partial_bundle()
