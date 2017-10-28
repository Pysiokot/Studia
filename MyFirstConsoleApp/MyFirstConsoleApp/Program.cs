using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MyFirstConsoleApp
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.Write("Type your blog name: ");
            var blogName = Console.ReadLine();

            Console.Write("Type your blog name: ");
            var blogName2 = Console.ReadLine();

            using (var db = new BlogContext())
            {
                var newBlog = new Blog()
                {
                    Name = blogName
                };

                var newBlog2 = new Blog()
                {
                    Name = blogName2
                };

                db.Blogs.Add(newBlog);
                db.Blogs.Add(newBlog2);
                db.SaveChanges();

                var query = from blog in db.Blogs
                            orderby blog.Name descending
                            select blog.Name;

                foreach (var item in query)
                {
                    Console.WriteLine(item);
                }

                var myForm = new BlogForm();
                myForm.ShowDialog();
            }
        }
    }
}
