package com.molkky.molkky.service;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.UserRepository;
import org.apache.commons.text.RandomStringGenerator;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.annotation.Rollback;

import javax.transaction.Transactional;

import static org.mockito.Mockito.*;

@SpringBootTest(classes = MolkkyApplication.class)
 class RegisterServiceTest {

    @Autowired
    RegisterService registerService;

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private EmailSenderService senderService;

    @Test
    @Rollback(false)
    @Transactional
    void saveUser(){
        User u = new User();
        u.setSurname("test register");

        u = registerService.saveUser(u);

        Assertions.assertEquals( "test register",u.getSurname(), "wrong name");
        Assertions.assertNotNull(u, "user not created");

    }

    @Test
    void testEncodeAndSendEmail() throws Exception {
        User user = new User();
        PasswordEncoder passwordEncoder = mock(PasswordEncoder.class);
        RandomStringGenerator pwdGenerator = new RandomStringGenerator.Builder().withinRange(33,63).build();
        String pwd = pwdGenerator.generate(10);

        user.setPassword(pwd);
        user.setEmail("test@gmail.com");
        registerService.encodeAndSendEmail(user);

        when(passwordEncoder.encode(pwd)).thenReturn("TEST");

        Assertions.assertEquals("TEST", passwordEncoder.encode(pwd));

        verify(passwordEncoder, times(1)).encode(pwd);
        verifyNoMoreInteractions(passwordEncoder);
    }
}
