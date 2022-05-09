package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.UserConnectionModel;
import com.molkky.molkky.repository.UserRepository;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@RunWith(SpringJUnit4ClassRunner.class)
public class ConnexionControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private ConnexionController connexionController;

    @Mock
    private UserRepository userRepository;

    @Mock
    private UserConnectionModel userModel;

    @Before
    public void setUp() {
        mockMvc = MockMvcBuilders.standaloneSetup(connexionController).build();
    }

    @Test
    public void testConnexionControllerWithoutUser() throws Exception {
        mockMvc.perform(get("/connexion/"))
                .andExpect(status().isOk());
        mockMvc.perform(post("/connexion/")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{\"email\": \"paco@sfr.fr\", \"password\": \"test\", \"code\": \"test\"}")
        )
                .andExpect(status().is3xxRedirection());
    }

    @Test
    public void testConnexionControllerWithUser() throws Exception {
        User user = new User();
        String mail = "test" + Math.random() * 10000 + "@gmail.com";
        String password = "Test2";

        Mockito.when(this.userModel.getEmail()).thenReturn(mail);
        Mockito.when(this.userModel.getPassword()).thenReturn(password);
        Mockito.when(this.userRepository.existsUserByEmailAndPassword(mail, password)).thenReturn(true);

        mockMvc.perform(get("/connexion/"))
                .andExpect(status().isOk());
        mockMvc.perform(post("/connexion/")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"email\":\"" + mail + "\", \"password\":\"" + password + "\", \"code\": \"test\"}")
                )
                .andExpect(status().is3xxRedirection());
    }
}
