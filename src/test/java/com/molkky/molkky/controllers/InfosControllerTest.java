package com.molkky.molkky.controllers;


import com.molkky.molkky.repository.UserTournamentRoleRepository;
import com.molkky.molkky.service.NotificationService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;

@WebMvcTest(value = InfosController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class InfosControllerTest {

    @Autowired
    private MockMvc mockMvc;
    @MockBean
    private NotificationService notificationService;
    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    private InfosController infosController;

    @Test
    void testInfosController() throws Exception {
        mockMvc.perform(get("/infos/"))
                .andExpect(status().isOk())
                .andExpect(view().name("infos"));
    }
}
