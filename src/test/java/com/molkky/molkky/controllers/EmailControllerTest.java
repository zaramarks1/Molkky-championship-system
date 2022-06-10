package com.molkky.molkky.controllers;

import com.molkky.molkky.service.EmailSenderService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;

@WebMvcTest(value = EmailController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class EmailControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    EmailSenderService emailSenderService;

    @Test
    void testShowButton() throws Exception {
        mockMvc.perform(get("/emailButton/"))
                .andExpect(status().isOk())
                .andExpect(view().name("/emailButton"));
    }

    @Test
    void testSendEmail() throws Exception {
        mockMvc.perform(post("/sendEmail/"))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:/emailButton"));

        emailSenderService.sendEmail("test@sfr.fr", "Test Subject", "Test");
        Mockito.verify(emailSenderService, Mockito.times(2)).sendEmail(Mockito.anyString(),Mockito.anyString(), Mockito.anyString());
        Mockito.verifyNoMoreInteractions(emailSenderService);
    }
}
