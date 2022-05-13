package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import com.molkky.molkky.service.NotificationService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = NotificationController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class NotificationControllerTest {
    @Autowired
    private MockMvc mockMvc;
    @MockBean
    private NotificationService notificationService;
    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @Test
    void markAsReadUnlogged() throws Exception {
        mockMvc.perform(post("/notifications/markAllAsRead"))
                .andExpect(status().isUnauthorized());
    }

    @Test
    void markAsReadLogged() throws Exception {
//        given
        UserLogged userLogged = Mockito.mock(UserLogged.class);
//        when
        when(userLogged.getTournamentRoleId()).thenReturn(1);
        when(userTournamentRoleRepository.findById(userLogged.getTournamentRoleId())).thenReturn(new UserTournamentRole());
//        then
        mockMvc.perform(post("/notifications/markAllAsRead").sessionAttr("user", userLogged))
                .andExpect(status().isOk());
        verify(notificationService, times(1)).markAllNotificationsAsRead(any());
    }
}
